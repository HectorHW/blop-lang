// this module defines api for working with objects from memory side

use super::objects::{
    OwnedObject, OwnedObjectItem, StackObject, StructDescriptor, StructInstance, VMap, VVec,
};
use crate::data::marked_counter::UNMARKED_ONE;
use crate::data::objects::{Closure, Partial, Value, ValueBox};
use crate::execution::arity::Arity;
use crate::execution::chunk::Chunk;
use crate::execution::vm::CallStackValue;
use std::pin::Pin;

const GC_YOUNG_THR_DEFAULT: usize = 100;
const GC_OLD_THR_DEFAULT: usize = 16000;
const GC_YOUNG_PASSES_DEFAULT: usize = 3;

pub struct GC {
    old_objects: Vec<Pin<Box<OwnedObject>>>,
    young_objects: Vec<Pin<Box<OwnedObject>>>,
    old_allocations: usize,
    pub new_allocations_threshold: usize,
    pub new_allocations_threshold_young: usize,
    gc_young_passes: usize,
}

impl StackObject {
    fn mark(&self, value: bool) {
        if let Some(obj) = self.unwrap_traceable() {
            obj.mark(value)
        }
    }
}

impl Clone for StackObject {
    fn clone(&self) -> Self {
        match self {
            StackObject::Int(i) => StackObject::Int(*i),
            StackObject::HeapObject(ptr) => {
                ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::HeapObject(*ptr)
            }

            StackObject::Builtin(s) => StackObject::Builtin(*s),
            &StackObject::BuiltinMethod {
                class_idx,
                method_idx,
            } => StackObject::BuiltinMethod {
                class_idx,
                method_idx,
            },
            StackObject::Blank => StackObject::Blank,
        }
    }
}

impl Drop for StackObject {
    fn drop(&mut self) {
        match self {
            StackObject::Int(_) => {}

            StackObject::HeapObject(ptr) => {
                ptr.unwrap_ref_mut().dec_gc_counter();
                #[cfg(feature = "debug-gc")]
                println!(
                    "drop stackobject of {:p}, RC is now {}",
                    ptr.unwrap_ref(),
                    ptr.unwrap_ref().marker.counter()
                );
            }
            StackObject::Builtin(..) | StackObject::Blank | Self::BuiltinMethod { .. } => {}
        }
    }
}

impl OwnedObject {
    fn make_stack_object(owned_reference: &mut OwnedObject) -> StackObject {
        use super::objects::PtrWrapper;
        let trace_ptr = owned_reference.wrap_private();
        StackObject::HeapObject(trace_ptr)
    }

    fn mark(&mut self, value: bool) {
        if self.marker.flag() == value {
            return;
        }
        self.marker.set_flag(value); //mark object itself

        match &self.item {
            OwnedObjectItem::Map(object) => {
                //mark children
                for entry in object {
                    entry.0.mark(value);
                    entry.1.mark(value);
                }
            }

            OwnedObjectItem::Vector(object) => {
                //mark children
                for vec_elem in object {
                    vec_elem.mark(value);
                }
            }
            OwnedObjectItem::ConstantString(_) => {} //has no children
            OwnedObjectItem::Box(ptr) => {
                ptr.0.mark(value);
            }
            OwnedObjectItem::Closure(c) => {
                c.underlying.mark(value);
                for closed_element in &c.closed_values {
                    closed_element.mark(value);
                }
            }

            OwnedObjectItem::Partial(partial) => {
                partial.target.mark(value);
                for stored_arg in &partial.args {
                    stored_arg.mark(value);
                }
            }

            OwnedObjectItem::Function(chunk) => {
                for constant in &chunk.constants {
                    constant.mark(value);
                }
            }
            OwnedObjectItem::StructDescriptor(d) => {
                for method in d.methods.values() {
                    method.mark(value);
                }
            }

            OwnedObjectItem::StructInstance(s) => {
                s.descriptor.mark(value);
                for field in s.fields.values() {
                    field.mark(value);
                }
            }
        }
    }

    fn clear_references(&mut self) -> bool {
        match &mut self.item {
            OwnedObjectItem::ConstantString(_) => false,

            OwnedObjectItem::Vector(v) => {
                let f = !v.is_empty();
                v.clear();
                f
            }
            OwnedObjectItem::Map(m) => {
                let f = !m.is_empty();
                m.clear();
                f
            }
            OwnedObjectItem::Box(b) => {
                if b.0 != StackObject::Int(0) {
                    b.0 = StackObject::Int(0);
                    return true;
                }
                false
            }
            OwnedObjectItem::Closure(c) => {
                let mut f = false;
                if let StackObject::Blank = c.underlying {
                } else {
                    let mut value = StackObject::Blank;
                    std::mem::swap(&mut c.underlying, &mut value);
                    drop(value);
                    f = true;
                }
                let f = f || !c.closed_values.is_empty();
                c.closed_values.clear();
                f
            }

            OwnedObjectItem::Partial(partial) => {
                let mut f = false;
                if let StackObject::Blank = partial.target {
                } else {
                    let mut value = StackObject::Blank;
                    std::mem::swap(&mut partial.target, &mut value);
                    drop(value);
                    f = true;
                }

                if !partial.args.is_empty() {
                    f = true;
                    partial.args.clear();
                }
                f
            }

            OwnedObjectItem::Function(chunk) => {
                let f = chunk.constants.is_empty();
                chunk.constants.clear();
                f
            }

            OwnedObjectItem::StructDescriptor(d) => {
                let f = d.methods.is_empty();
                d.methods.clear();
                f
            }
            OwnedObjectItem::StructInstance(s) => {
                s.descriptor = StackObject::Int(0);
                s.fields.clear();
                true
            }
        }
    }

    fn inc_gc_counter(&mut self) {
        self.marker.inc()
    }

    fn dec_gc_counter(&mut self) {
        self.marker.dec()
    }

    fn mark_shallow(&mut self, value: bool) {
        self.marker.set_flag(value);
    }

    fn is_marked(&self) -> bool {
        self.marker.flag()
    }
}

pub trait GCAlloc: Sized {
    fn needs_gc() -> bool;

    fn store(_obj: Self) -> OwnedObject {
        panic!("store on non-gc object")
    } // for objects that need GC

    fn make(_obj: Self) -> StackObject {
        panic!("make on GC object")
    } //for objects that dont need GC
}

pub trait GCNew: GCAlloc + Default {
    fn allocate_new() -> OwnedObject {
        Self::store(Self::default())
    } // for objects that need GC

    fn make_new() -> StackObject {
        Self::make(Self::default())
    }
}

impl GCAlloc for i64 {
    fn needs_gc() -> bool {
        false
    }

    fn make(obj: Self) -> StackObject {
        StackObject::Int(obj)
    }
}

impl GCNew for i64 {}

impl GCAlloc for VMap {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Map(obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCNew for VMap {}

impl GCAlloc for VVec {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Vector(obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCNew for VVec {}

impl GCAlloc for String {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::ConstantString(obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCNew for String {}

impl GCNew for ValueBox {}

impl GCAlloc for ValueBox {
    fn needs_gc() -> bool {
        true
    }

    fn store(_obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Box(_obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCAlloc for Closure {
    fn needs_gc() -> bool {
        true
    }

    fn store(_obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Closure(_obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCAlloc for Partial {
    fn needs_gc() -> bool {
        true
    }

    fn store(_obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Partial(_obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCAlloc for Chunk {
    fn needs_gc() -> bool {
        true
    }

    fn store(_obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Function(_obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCAlloc for StructDescriptor {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::StructDescriptor(obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCAlloc for StructInstance {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::StructInstance(obj),
            marker: UNMARKED_ONE,
        }
    }
}

#[cfg(feature = "debug-gc")]
impl Drop for OwnedObject {
    fn drop(&mut self) {
        println!("drop {:p}", self);
    }
}

/// hybrid garbage collector that implements mark-and-sweep algorithm with reference counting.
/// Creation and clear methods are marked as unsafe because misuse of this structure may create
/// dangling pointers or access to freed memory. You MUST drop all objects that contain GCrefs
/// before dropping instance of GC.
///
impl GC {
    /// creates new instance of GC.
    ///
    /// # Arguments
    /// * `thr` - threshhold of old allocations. This many allocations of old objects will trigger
    /// mark and sweeep algorithm
    /// * `thr_young` - threshhold of new allocations. This may allocations wil trigger quck pass
    /// * `young_passes` - amount of passes over young object in attempt to free more objects
    pub unsafe fn new(thr: usize, thr_young: usize, young_passes: usize) -> Self {
        GC {
            old_objects: Vec::new(),
            young_objects: Vec::new(),
            old_allocations: 0,
            new_allocations_threshold: thr,
            new_allocations_threshold_young: thr_young,
            gc_young_passes: young_passes,
        }
    }
    ///create instance of GC with default config (see GC_YOUNG_PASSES_DEFAULT, GC_YOUNG_THR_DEFAULT
    /// and GC_OLD_THR_DEFAULT)
    pub unsafe fn default_gc() -> Self {
        let young_thr = if cfg!(feature = "debug-gc") {
            10
        } else {
            GC_YOUNG_THR_DEFAULT
        };
        let old_thr = if cfg!(feature = "debug-gc") {
            10
        } else {
            GC_OLD_THR_DEFAULT
        };
        Self::new(old_thr, young_thr, GC_YOUNG_PASSES_DEFAULT)
    }

    pub fn allocate_new<T: GCNew>(&mut self) -> StackObject {
        self.store(T::default())
    }

    pub fn store<T: GCAlloc>(&mut self, item: T) -> StackObject {
        if T::needs_gc() {
            let obj = T::store(item);
            let boxed = Box::new(obj);
            self.young_objects.push(Pin::new(boxed));

            let box_ref = self.young_objects.last_mut().unwrap(); //obj is not null

            OwnedObject::make_stack_object(box_ref)
        } else {
            T::make(item)
        }
    }

    /// quick check to determine if we need to trigger any stage of garbage collection
    pub fn needs_collection(&self) -> bool {
        self.young_objects.len() >= self.new_allocations_threshold_young
    }

    /// (maybe) collects garbage from internal list of objects (created with allocate_new or store)
    ///
    /// # Arguments
    ///
    /// * `iter` - An iterator over roots to mark (stack, VM's constants storage and so on)
    /// * `chunks` - chunks of code. They need to be visited too as they may contain gc refs in
    /// constants
    ///
    /// thin function is unsafe because passing an iterator that does not include all possible items
    /// will create dangling pointers
    pub unsafe fn mark_and_sweep<'a, I>(&mut self, iter: I, call_stack: &[CallStackValue])
    where
        I: Iterator<Item = &'a StackObject>,
    {
        if !self.needs_collection() {
            return;
        }

        self.quick_pass();

        if self.old_allocations < self.new_allocations_threshold {
            return;
        }

        debug_assert!(self.young_objects.is_empty());

        #[cfg(feature = "debug-gc")]
        println!("begin slow_pass");

        //mark
        for item in iter {
            item.mark(true);
        }

        for stack_frame in call_stack {
            let chunk = &stack_frame.return_chunk;
            chunk.mark(true);
        }

        //clean refs
        for obj in &mut self.old_objects {
            if !obj.is_marked() {
                #[cfg(feature = "debug-gc")]
                println!("long pass: cleared refs in {:p}", obj.as_ref());
                obj.clear_references();
            }
        }

        //sweep - drop unmarked objects
        self.old_objects.retain(|obj| obj.is_marked());

        for item in &mut self.old_objects {
            item.mark_shallow(false);
        }

        self.old_allocations = 0;
        #[cfg(feature = "debug-gc")]
        println!("end slow_pass");
    }

    unsafe fn quick_pass(&mut self) {
        #[cfg(feature = "debug-gc")]
        println!("begin quick_pass");
        if self.gc_young_passes == 0 {
            loop {
                let before = self.young_objects.len();
                self.gc_quick_pass_step();
                let after = self.young_objects.len();
                if before == after {
                    break;
                }
            }
        } else {
            for _i in 0..self.gc_young_passes {
                #[cfg(feature = "debug-gc")]
                println!("quick pass step {}", _i);
                let before = self.young_objects.len();
                self.gc_quick_pass_step();
                let after = self.young_objects.len();
                if before == after {
                    break;
                }
            }
        }

        //move young objects into old
        self.old_allocations += self.young_objects.len();
        #[cfg(feature = "debug-gc")]
        println!("added {} old object(s)", self.young_objects.len());
        self.old_objects.append(&mut self.young_objects);
        #[cfg(feature = "debug-gc")]
        println!("end quick_pass");
    }

    unsafe fn gc_quick_pass_step(&mut self) {
        //drop young objects whose RC is zero
        self.young_objects.retain(|obj| {
            #[cfg(feature = "debug-gc")]
            println!(
                "retain check on {:p}, RC is {}",
                obj.as_ref(),
                obj.marker.counter()
            );
            obj.marker.counter() > 0
        });
    }

    pub fn clone_value(&mut self, obj: &StackObject) -> StackObject {
        //copies underlying object
        match obj {
            StackObject::Int(i) => StackObject::Int(*i), //no cloning necessary

            h @ StackObject::HeapObject(ptr) => {
                if matches!(ptr.unwrap_ref().item, OwnedObjectItem::ConstantString(..)) {
                    h.clone()
                } else {
                    let new_obj = ptr.unwrap_ref().clone();
                    let boxed = Pin::new(Box::new(new_obj));
                    self.young_objects.push(boxed);
                    let mut_ref = self.old_objects.last_mut().unwrap();
                    OwnedObject::make_stack_object(mut_ref)
                }
            }

            b @ StackObject::Builtin(..) => b.clone(),
            b @ StackObject::BuiltinMethod { .. } => b.clone(),
            StackObject::Blank => StackObject::Blank,
        }
    }

    pub fn len(&self) -> usize {
        self.old_objects.len()
    }

    pub fn new_string(&mut self, s: &str) -> StackObject {
        self.store(s.to_string())
    }

    pub fn new_const_string(&mut self, s: &str) -> StackObject {
        for item in &mut self.young_objects {
            match &item.item {
                OwnedObjectItem::ConstantString(obj) if obj.as_str() == s => {
                    #[cfg(feature = "debug-gc")]
                    println!(
                        "found string {} in young objects [{:p}] with RC = {}",
                        s,
                        item.as_ref(),
                        item.marker.counter()
                    );
                    let ptr = OwnedObject::make_stack_object(item);
                    item.inc_gc_counter();
                    #[cfg(feature = "debug-gc")]
                    println!("new RC is {}", item.marker.counter());
                    return ptr;
                }
                _ => {}
            }
        }

        for item in &mut self.old_objects {
            match &item.item {
                OwnedObjectItem::ConstantString(obj) if obj.as_str() == s => {
                    #[cfg(feature = "debug-gc")]
                    println!(
                        "found string {} in old objects [{:p}] with RC = {}",
                        s,
                        item.as_ref(),
                        item.marker.counter()
                    );
                    let ptr = OwnedObject::make_stack_object(item);
                    item.inc_gc_counter();
                    #[cfg(feature = "debug-gc")]
                    println!("new RC is {}", item.marker.counter());
                    return ptr;
                }
                _ => {}
            }
        }

        self.store(s.to_string())
    }

    pub(crate) fn new_partial(
        &mut self,
        target: Value,
        arity: Arity,
        args: Vec<Value>,
    ) -> StackObject {
        self.store(Partial::new(target, arity, args))
    }

    pub fn try_inplace_string_concat(
        &mut self,
        s1: StackObject,
        s2: StackObject,
    ) -> Result<StackObject, String> {
        if s1.unwrap_any_str().is_none() {
            return Err(format!(
                "first argument of string concatenation is not string-like (got {})",
                s1.type_string()
            ));
        }

        if s2.unwrap_any_str().is_none() {
            return Err(format!(
                "second argument of string concatenation is not string-like (got {})",
                s2.type_string()
            ));
        }

        #[cfg(feature = "debug-gc")]
        println!("try_inplace_string_concat");

        match (&mut s1.as_heap_object(), &mut s2.as_heap_object()) {
            (Some(_obj1), _obj2)
                if s1.unwrap_traceable().unwrap().marker.counter() == 1
                    && s1.unwrap_mutable_string().is_some() =>
            {
                #[cfg(feature = "debug-gc")]
                println!(
                    "RC of s1[{:p}] is 1, reusing it",
                    s1.unwrap_traceable().unwrap()
                );
                s1.unwrap_mutable_string()
                    .unwrap()
                    .push_str(s2.unwrap_any_str().unwrap());
                return Ok(s1);
            }

            (_obj1, Some(_obj2))
                if s2.unwrap_traceable().unwrap().marker.counter() == 1
                    && s2.unwrap_mutable_string().is_some() =>
            {
                #[cfg(feature = "debug-gc")]
                println!(
                    "RC of s2[{:p}] is 1, reusing it",
                    s2.unwrap_traceable().unwrap()
                );
                let mut part_1 = s1.unwrap_any_str().unwrap().to_string();
                std::mem::swap(&mut part_1, s2.unwrap_mutable_string().unwrap());
                s2.unwrap_mutable_string().unwrap().push_str(&part_1);
                return Ok(s2);
            }
            (_, _) => {}
        };
        #[cfg(feature = "debug-gc")]
        println!(
            "RC(s1[{:p}])={}, RC(s2[{:p}])={}, creating new",
            s1.unwrap_traceable().unwrap(),
            s1.unwrap_traceable().unwrap().marker.counter(),
            s2.unwrap_traceable().unwrap(),
            s2.unwrap_traceable().unwrap().marker.counter()
        );

        let result_string = self.allocate_new::<String>();

        result_string
            .unwrap_mutable_string()
            .unwrap()
            .push_str(s1.unwrap_any_str().unwrap());

        result_string
            .unwrap_mutable_string()
            .unwrap()
            .push_str(s2.unwrap_any_str().unwrap());

        Ok(result_string)
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = &'_ Pin<Box<OwnedObject>>> {
        self.young_objects.iter().chain(self.old_objects.iter())
    }
}

impl Drop for GC {
    fn drop(&mut self) {
        #[cfg(feature = "debug-gc")]
        println!("begin drop gc");
        //clean refs
        for obj in &mut self.young_objects {
            obj.clear_references();
        }
        for obj in &mut self.old_objects {
            obj.clear_references();
        }
        #[cfg(feature = "debug-gc")]
        println!("end drop gc");
    }
}
