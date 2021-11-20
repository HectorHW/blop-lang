// this module defines api for working with objects from memory side

use super::objects::{OwnedObject, OwnedObjectItem, StackObject, VMap, VVec};
use crate::data::marked_counter::{MarkedCounter, UNMARKED_ONE};
use crate::data::objects::{Closure, ValueBox};
use crate::execution::chunk::Chunk;
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
        match self {
            StackObject::Function { .. } => {
                //chunks are marked separately by gc
            }

            _ => {
                if let Some(obj) = self.unwrap_traceable() {
                    obj.mark(value)
                }
            }
        }
    }
}

impl Clone for StackObject {
    fn clone(&self) -> Self {
        match self {
            StackObject::Int(i) => StackObject::Int(*i),
            StackObject::Function { chunk_id } => StackObject::Function {
                chunk_id: *chunk_id,
            },

            StackObject::Map(gc_ptr, obj_ptr) => {
                gc_ptr.unwrap_ref_mut().inc_gc_counter();
                return StackObject::Map(*gc_ptr, *obj_ptr);
            }
            StackObject::Vector(gc_ptr, obj_ptr) => {
                gc_ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::Vector(*gc_ptr, *obj_ptr)
            }
            StackObject::MutableString(gc_ptr, obj_ptr) => {
                gc_ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::MutableString(*gc_ptr, *obj_ptr)
            }
            StackObject::ConstantString(gc_ptr, obj_ptr) => {
                gc_ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::ConstantString(*gc_ptr, *obj_ptr)
            }
            StackObject::Box(gc_ptr, obj_ptr) => {
                gc_ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::Box(*gc_ptr, *obj_ptr)
            }
            StackObject::Closure(gc_ptr, obj_ptr) => {
                gc_ptr.unwrap_ref_mut().inc_gc_counter();
                StackObject::Closure(*gc_ptr, *obj_ptr)
            }
        }
    }
}

impl Drop for StackObject {
    fn drop(&mut self) {
        match self {
            StackObject::Int(_) => {}
            StackObject::Function { .. } => {}
            StackObject::Map(gc_ptr, _)
            | StackObject::Vector(gc_ptr, _)
            | StackObject::MutableString(gc_ptr, _)
            | StackObject::ConstantString(gc_ptr, _)
            | StackObject::Box(gc_ptr, _)
            | StackObject::Closure(gc_ptr, _) => {
                gc_ptr.unwrap_ref_mut().dec_gc_counter();
                #[cfg(feature = "debug-gc")]
                println!(
                    "drop stackobject of {:p}, RC is now {}",
                    gc_ptr.unwrap_ref(),
                    gc_ptr.unwrap_ref().marker.counter()
                );
            }
        }
    }
}

impl OwnedObject {
    fn make_stack_object(owned_reference: &mut OwnedObject) -> StackObject {
        use super::objects::PtrWrapper;
        let trace_ptr = owned_reference.wrap_private();

        match &mut owned_reference.item {
            OwnedObjectItem::Map(ptr) => {
                let object_ptr = ptr.wrap_private();
                StackObject::Map(trace_ptr, object_ptr)
            }
            OwnedObjectItem::Vector(v) => {
                let object_ptr = v.wrap_private();
                StackObject::Vector(trace_ptr, object_ptr)
            }
            OwnedObjectItem::MutableString(s) => {
                let object_ptr = s.wrap_private();
                StackObject::MutableString(trace_ptr, object_ptr)
            }

            OwnedObjectItem::ConstantString(s) => {
                let object_ptr = s.wrap_private();
                StackObject::ConstantString(trace_ptr, object_ptr)
            }
            OwnedObjectItem::Box(ptr) => {
                let object_ptr = ptr.wrap_private();
                StackObject::Box(trace_ptr, object_ptr)
            }
            OwnedObjectItem::Closure(ptr) => {
                let object_ptr = ptr.wrap_private();
                StackObject::Closure(trace_ptr, object_ptr)
            }
        }
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
            OwnedObjectItem::MutableString(_) | OwnedObjectItem::ConstantString(_) => {} //has no children
            OwnedObjectItem::Box(ptr) => {
                ptr.0.mark(value);
            }
            OwnedObjectItem::Closure(Closure { closed_values, .. }) => {
                for closed_element in closed_values {
                    closed_element.mark(value);
                }
            }
        }
    }

    fn clear_references(&mut self) -> bool {
        match &mut self.item {
            OwnedObjectItem::ConstantString(_) => false,
            OwnedObjectItem::MutableString(_) => false,

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
                let f = !c.closed_values.is_empty();
                c.closed_values.clear();
                f
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
            item: OwnedObjectItem::MutableString(obj),
            marker: UNMARKED_ONE,
        }
    }
}

impl GCNew for String {}

struct _ConstHeapString(String);

impl GCAlloc for _ConstHeapString {
    fn needs_gc() -> bool {
        true
    }

    fn store(_obj: Self) -> OwnedObject {
        OwnedObject {
            item: OwnedObjectItem::ConstantString(_obj.0),
            marker: UNMARKED_ONE,
        }
    }
}
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

#[cfg(feature = "debug-gc")]
impl Drop for OwnedObject {
    fn drop(&mut self) {
        println!("drop {:p}", self);
    }
}

impl GC {
    pub fn new(thr: usize, thr_young: usize, young_passes: usize) -> Self {
        GC {
            old_objects: Vec::new(),
            young_objects: Vec::new(),
            old_allocations: 0,
            new_allocations_threshold: thr,
            new_allocations_threshold_young: thr_young,
            gc_young_passes: young_passes,
        }
    }

    pub fn default_gc() -> Self {
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

    /// (maybe) collects garbage from internal list of objects (created with allocate_new or store)
    ///
    /// # Arguments
    ///
    /// * `iter` - An iterator over roots to mark (stack, VM's constants storage and so on)
    ///
    /// thin function is unsafe because passing an iterator that does not include all possible items
    /// will create dangling pointers
    pub unsafe fn mark_and_sweep<'a, I>(&mut self, iter: I, chunks: &[Chunk])
    where
        I: Iterator<Item = &'a StackObject>,
    {
        if self.young_objects.len() < self.new_allocations_threshold_young {
            return;
        }

        self.quick_pass();

        if self.old_allocations < self.new_allocations_threshold {
            return;
        }

        #[cfg(feature = "debug-gc")]
        println!("begin slow_pass");

        //mark
        for item in iter {
            item.mark(true);
        }

        for chunk in chunks {
            let _ = chunk.constants.iter().map(|obj| obj.mark(true));
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

        for chunk in chunks {
            let _ = chunk.constants.iter().map(|obj| obj.mark(false));
        }
        self.old_allocations = 0;
        #[cfg(feature = "debug-gc")]
        println!("end slow_pass");
    }

    /*
    return value indicates if actually cleared references which could create
    more objects for deletion
     */
    fn clear_young_references_pass(&mut self) -> bool {
        let mut flag = false;
        for item in &mut self.young_objects {
            if item.marker.counter() == 0 && item.clear_references() {
                flag = true;
                #[cfg(feature = "debug-gc")]
                println!("cleared refs in {:p}", item.as_ref());
            }
        }
        flag
    }

    unsafe fn quick_pass(&mut self) {
        #[cfg(feature = "debug-gc")]
        println!("begin quick_pass");
        //clear references in objects that will be deleted.
        //this action may create more objects to clear
        if self.gc_young_passes == 0 {
            loop {
                if !self.clear_young_references_pass() {
                    break;
                }
            }
        } else {
            for _ in 0..self.gc_young_passes {
                if !self.clear_young_references_pass() {
                    break;
                }
            }
        }

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
        //move young objects into old
        self.old_allocations += self.young_objects.len();
        #[cfg(feature = "debug-gc")]
        println!("added {} old object(s)", self.young_objects.len());
        self.old_objects.append(&mut self.young_objects);
        #[cfg(feature = "debug-gc")]
        println!("end quick_pass");
    }

    pub fn clone_value(&mut self, obj: &StackObject) -> StackObject {
        //copies underlying object
        match obj {
            StackObject::Int(i) => StackObject::Int(*i), //no cloning necessary

            StackObject::Function { chunk_id } => StackObject::Function {
                chunk_id: *chunk_id,
            }, //no cloning necessary for function as it itself carries no data that can change during runtime

            s @ StackObject::ConstantString(..) => {
                s.clone()
                //constant string are *cough cough* constant, no need to add new object,
                // just reuse old one, but bump counter
            }

            StackObject::MutableString(..)
            | StackObject::Vector(..)
            | StackObject::Map(..)
            | StackObject::Closure(..)
            | StackObject::Box(..) => {
                let owned_ref = obj.unwrap_traceable().expect("null ptr in clone");
                let new_obj = owned_ref.clone();
                let obj_boxed = Box::new(new_obj);
                self.old_objects.push(Pin::new(obj_boxed));

                let mut_ref = self.old_objects.last_mut().unwrap();

                OwnedObject::make_stack_object(mut_ref)
            }
        }
    }

    pub fn len(&self) -> usize {
        self.old_objects.len()
    }

    pub unsafe fn clear(&mut self) {
        self.old_objects.clear()
    }

    pub fn new_string(&mut self, s: &str) -> StackObject {
        self.store(s.to_string())
    }

    pub fn new_const_string(&mut self, s: &str) -> StackObject {
        //TODO: interning?
        self.store(_ConstHeapString(s.to_string()))
    }
}

impl Drop for GC {
    fn drop(&mut self) {
        //clean refs
        for obj in &mut self.young_objects {
            obj.clear_references();
        }
        for obj in &mut self.old_objects {
            obj.clear_references();
        }
    }
}
