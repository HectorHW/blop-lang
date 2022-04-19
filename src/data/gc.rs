// this module defines api for working with objects from memory side

use nohash_hasher::IntMap;

use super::objects::{
    EnumDescriptor, OwnedObject, OwnedObjectItem, StackObject, StructDescriptor, StructInstance,
    VMap, VVec,
};
use super::short_string::ShortString;
use crate::data::marked_counter::UNMARKED_ONE;
use crate::data::objects::{Closure, Partial, Value, ValueBox, SHORT_STRING_BUF_SIZE};
use crate::execution::arity::Arity;
use crate::execution::chunk::Chunk;
use crate::execution::vm::CallStackValue;
use std::pin::Pin;
use std::ptr::NonNull;

const GC_THR_DEFAULT: usize = 1000;

pub struct GC {
    objects: IntMap<usize, Pin<Box<OwnedObject>>>,
    allocations: usize,
    pub allocations_threshold: usize,
    grow_factor: f64,
    is_cleaning: bool,
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
            &StackObject::Int(i) => StackObject::Int(i),
            &StackObject::Float(f) => StackObject::Float(f),
            &StackObject::Bool(b) => StackObject::Bool(b),
            &StackObject::Nothing => StackObject::Nothing,
            &StackObject::ShortString(s) => StackObject::ShortString(s),
            StackObject::HeapObject(ptr) => {
                ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::HeapObject(*ptr)
            }

            &StackObject::Builtin(s) => StackObject::Builtin(s),
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
        #[cfg(feature = "verbose-gc")]
        let type_ = self.type_string();
        match self {
            StackObject::Int(_)
            | StackObject::Bool(..)
            | StackObject::Float(..)
            | StackObject::Nothing => {}
            StackObject::ShortString(..) => {}

            StackObject::HeapObject(ptr) => {
                ptr.unwrap_ref_mut().dec_gc_counter();
                #[cfg(feature = "verbose-gc")]
                println!(
                    "drop stackobject of {:p} ({}), RC is now {}",
                    ptr.unwrap_ref(),
                    type_,
                    ptr.unwrap_ref().marker.counter()
                );

                if ptr.unwrap_ref_mut().get_gc_counter() == 0 {
                    //no pointers left
                    #[cfg(feature = "verbose-gc")]
                    println!("reached 0 refs, requesting drop");
                    unsafe {
                        let ptr = ptr.unwrap_ref_mut();
                        let addr = GC::get_addressable_index(ptr);
                        let gc = ptr.get_gc();

                        gc.drop_notify(addr);
                    }
                }
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

                if let Some(e) = d.enum_ref.as_ref() {
                    e.mark(value)
                }
            }

            OwnedObjectItem::EnumDescriptor(d) => {
                for variant in d.variants.values() {
                    variant.mark(value);
                }

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
        #[cfg(feature = "verbose-gc")]
        let ptr = self as *const _ as usize;
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
                let f = !d.methods.is_empty() || d.enum_ref.is_some();
                #[cfg(feature = "verbose-gc")]
                println!(
                    "taking enum_ref in {ptr:x} which is {}",
                    d.enum_ref.is_some()
                );
                let _ = d.enum_ref.take();
                #[cfg(feature = "verbose-gc")]
                println!("clearing methods in struct {ptr:#x}");
                d.methods.clear();
                f
            }

            OwnedObjectItem::EnumDescriptor(d) => {
                let f = !d.methods.is_empty() || !d.variants.is_empty();
                #[cfg(feature = "verbose-gc")]
                println!("clearing methods in enum {ptr:#x}");
                d.methods.clear();
                #[cfg(feature = "verbose-gc")]
                println!("clearing variants in enum {ptr:#x}");
                d.variants.clear();
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

    fn get_gc_counter(&self) -> usize {
        self.marker.counter()
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

    fn store(_obj: Self, _gc: &mut GC) -> OwnedObject {
        panic!("store on non-gc object")
    } // for objects that need GC

    fn make(_obj: Self) -> StackObject {
        panic!("make on GC object")
    } //for objects that dont need GC
}

pub trait GCNew: GCAlloc + Default {
    fn allocate_new(gc: &mut GC) -> OwnedObject {
        Self::store(Self::default(), gc)
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

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Map(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCNew for VMap {}

impl GCAlloc for VVec {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Vector(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCNew for VVec {}

impl GCAlloc for String {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::ConstantString(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCNew for String {}

impl GCNew for ValueBox {}

impl GCAlloc for ValueBox {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Box(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCAlloc for Closure {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Closure(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCAlloc for Partial {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Partial(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCAlloc for Chunk {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::Function(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCAlloc for StructDescriptor {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::StructDescriptor(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCAlloc for EnumDescriptor {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::EnumDescriptor(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

impl GCAlloc for StructInstance {
    fn needs_gc() -> bool {
        true
    }

    fn store(obj: Self, gc: &mut GC) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::StructInstance(obj),
            marker: UNMARKED_ONE,
            owning_gc: NonNull::from(gc),
        }
    }
}

#[cfg(feature = "verbose-gc")]
impl Drop for OwnedObject {
    fn drop(&mut self) {
        println!("drop {:p} ({})", self, self.type_string());
    }
}

impl OwnedObject {
    pub(super) unsafe fn get_gc(&mut self) -> &mut GC {
        self.owning_gc.as_mut()
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
    /// * `thr` - threshhold of allocations. This many allocations of objects will trigger
    /// mark and sweeep algorithm
    pub unsafe fn new(thr: usize) -> Self {
        GC {
            objects: Default::default(),
            allocations: 0,
            allocations_threshold: thr,
            is_cleaning: false,
            grow_factor: 1.2f64,
        }
    }
    ///create instance of GC with default config (see GC_THR_DEFAULT)
    pub unsafe fn default_gc() -> Self {
        let thr = if cfg!(feature = "debug-gc") {
            10
        } else {
            GC_THR_DEFAULT
        };
        Self::new(thr)
    }

    pub fn allocate_new<T: GCNew>(&mut self) -> StackObject {
        self.store(T::default())
    }

    pub fn store<T: GCAlloc>(&mut self, item: T) -> StackObject {
        if T::needs_gc() {
            let obj = T::store(item, self);
            let mut boxed = Box::new(obj);

            let stack_ptr = OwnedObject::make_stack_object(boxed.as_mut());

            let index = GC::get_addressable_index(boxed.as_mut());

            self.objects.insert(index, Pin::new(boxed));

            self.allocations += 1;

            stack_ptr
        } else {
            T::make(item)
        }
    }

    /// quick check to determine if we need to trigger any stage of garbage collection
    pub fn needs_collection(&self) -> bool {
        self.allocations >= self.allocations_threshold
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

        self.is_cleaning = true;

        //clean refs
        for obj in &mut self.objects.values_mut() {
            if !obj.is_marked() {
                #[cfg(feature = "verbose-gc")]
                println!("long pass: cleared refs in {:p}", obj.as_ref());
                obj.clear_references();
            }
        }

        self.is_cleaning = false;

        //sweep - drop unmarked objects
        self.objects.retain(|_, obj| obj.is_marked());

        self.allocations = self.objects.len();

        for item in &mut self.objects.values_mut() {
            item.as_mut().mark_shallow(false);
        }

        let new_thr = (self.objects.len() as f64 * self.grow_factor).ceil() as usize;

        self.allocations_threshold = usize::max(new_thr, self.allocations_threshold);

        #[cfg(feature = "debug-gc")]
        println!("end slow_pass");
    }

    /// drop object identified by address `addr` (probably produced by GC::get_addressable_index)
    ///
    /// This function is unsafe because in non-debug environment existence of pointers to named
    /// object is not checked, which may lead to dropping memory that is still referenced somewhere
    pub unsafe fn drop_notify(&mut self, addr: usize) {
        if self.is_cleaning {
            return; //do not drop objects inside clear
        }
        let object = self.objects.remove(&addr).unwrap();
        debug_assert!(object.get_gc_counter() == 0);
        drop(object);
        self.allocations -= 1;
    }

    pub fn clone_value(&mut self, obj: &StackObject) -> StackObject {
        //copies underlying object
        match obj {
            s @ StackObject::Int(..)
            | s @ StackObject::Float(..)
            | s @ StackObject::Bool(..)
            | s @ StackObject::Nothing
            | s @ StackObject::ShortString(..) => s.clone(), //no cloning necessary

            h @ StackObject::HeapObject(ptr) => {
                if matches!(ptr.unwrap_ref().item, OwnedObjectItem::ConstantString(..)) {
                    h.clone()
                } else {
                    let mut boxed = Box::new(ptr.unwrap_ref().clone());

                    let stack_ptr = OwnedObject::make_stack_object(boxed.as_mut());

                    let index = GC::get_addressable_index(boxed.as_mut());

                    self.objects.insert(index, Pin::new(boxed));

                    self.allocations += 1;

                    stack_ptr
                }
            }

            b @ StackObject::Builtin(..) => b.clone(),
            b @ StackObject::BuiltinMethod { .. } => b.clone(),
            StackObject::Blank => StackObject::Blank,
        }
    }

    pub fn len(&self) -> usize {
        self.objects.len()
    }

    pub fn new_string(&mut self, s: &str) -> StackObject {
        if let Some(ss) = ShortString::<8>::try_new(s) {
            return StackObject::ShortString(ss);
        }

        self.store(s.to_string())
    }

    pub fn new_interned_string(&mut self, s: &str) -> StackObject {
        if let Some(ss) = ShortString::<8>::try_new(s) {
            return StackObject::ShortString(ss);
        }

        for item in &mut self.objects.values_mut() {
            match &item.item {
                OwnedObjectItem::ConstantString(obj) if obj.as_str() == s => {
                    #[cfg(feature = "verbose-gc")]
                    println!(
                        "found string {} in old objects [{:p}] with RC = {}",
                        s,
                        item.as_ref(),
                        item.marker.counter()
                    );
                    let ptr = OwnedObject::make_stack_object(item);
                    item.inc_gc_counter();
                    #[cfg(feature = "verbose-gc")]
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

        if let Some(ss) = ShortString::<SHORT_STRING_BUF_SIZE>::try_concat(
            s1.unwrap_any_str().unwrap(),
            s2.unwrap_any_str().unwrap(),
        ) {
            return Ok(StackObject::ShortString(ss));
        }

        #[cfg(feature = "verbose-gc")]
        println!("try_inplace_string_concat");

        match (&mut s1.as_heap_object(), &mut s2.as_heap_object()) {
            (Some(_obj1), _obj2)
                if s1.unwrap_traceable().unwrap().marker.counter() == 1
                    && s1.unwrap_mutable_string().is_some() =>
            {
                #[cfg(feature = "verbose-gc")]
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
                #[cfg(feature = "verbose-gc")]
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
        #[cfg(feature = "verbose-gc")]
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
        self.objects.values()
    }

    pub fn get_addressable_index(object: &mut OwnedObject) -> usize {
        let addr_value = object as *const _ as usize;

        let multiplier = std::mem::align_of::<OwnedObject>();

        //if this assert fails Rust is probably broken (refs are aligned)
        debug_assert_eq!(addr_value % multiplier, 0);
        addr_value / multiplier
    }
}

impl Drop for GC {
    fn drop(&mut self) {
        #[cfg(feature = "debug-gc")]
        println!("begin drop gc");
        self.is_cleaning = true;
        //clean refs
        for obj in self.objects.values_mut() {
            #[cfg(feature = "verbose-gc")]
            println!("working on {:p}", obj.as_mut());
            obj.clear_references();
        }
        self.is_cleaning = false;
        #[cfg(feature = "debug-gc")]
        println!("end drop gc");
    }
}
