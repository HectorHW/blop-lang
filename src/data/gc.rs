// this module defines api for working with objects from memory side

use super::objects::{OwnedObject, OwnedObjectItem, StackObject, VMap, VVec};
use crate::execution::chunk::Chunk;
use std::pin::Pin;

pub struct GC {
    objects: Vec<Pin<Box<OwnedObject>>>,
    new_allocations: usize,
    pub new_allocations_threshold: usize,
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
        }
    }

    fn mark(&mut self, value: bool) {
        if self.marker == value {
            return;
        }
        self.marker = value; //mark object itself

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
        }
    }

    fn mark_shallow(&mut self, value: bool) {
        self.marker = value;
    }

    fn is_marked(&self) -> bool {
        self.marker
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
            marker: false,
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
            marker: false,
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
            marker: false,
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
            marker: false,
        }
    }
}

impl GC {
    pub fn new(thr: usize) -> Self {
        GC {
            objects: Vec::new(),
            new_allocations: 0,
            new_allocations_threshold: thr,
        }
    }

    pub fn allocate_new<T: GCNew>(&mut self) -> StackObject {
        self.store(T::default())
    }

    pub fn store<T: GCAlloc>(&mut self, item: T) -> StackObject {
        if T::needs_gc() {
            self.new_allocations += 1;
            let obj = T::store(item);
            let boxed = Box::new(obj);
            self.objects.push(Pin::new(boxed));

            let box_ref = self.objects.last_mut().unwrap(); //obj is not null

            return OwnedObject::make_stack_object(box_ref);
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
        if self.new_allocations < self.new_allocations_threshold {
            return;
        }

        //mark
        for item in iter {
            item.mark(true);
        }

        for chunk in chunks {
            let _ = chunk.constants.iter().map(|obj| obj.mark(true));
        }

        //sweep - drop unmarked objects
        self.objects.retain(|obj| obj.is_marked());

        for item in &mut self.objects {
            item.mark_shallow(false);
        }

        for chunk in chunks {
            let _ = chunk.constants.iter().map(|obj| obj.mark(false));
        }

        //let's hope some day Vec.drain_filter() will be accessible
        //mark shallow 479 at thr=16000
        //mark 456
    }

    pub fn clone_value(&mut self, obj: &StackObject) -> StackObject {
        //copies underlying object
        match obj {
            StackObject::Int(i) => StackObject::Int(*i), //no cloning necessary

            StackObject::Function { chunk_id } => StackObject::Function {
                chunk_id: *chunk_id,
            }, //no cloning necessary for function as it itself carries no data that can change during runtime

            StackObject::ConstantString(ptr1, ptr2) => {
                StackObject::ConstantString(*ptr1, *ptr2)
                //constant string are *cough cough* counstant, no need to add new object, just reuse old one
            }

            StackObject::MutableString(..) | StackObject::Vector(..) | StackObject::Map(..) => {
                let owned_ref = obj.unwrap_traceable().expect("null ptr in clone");
                let new_obj = owned_ref.clone();
                let obj_boxed = Box::new(new_obj);
                self.objects.push(Pin::new(obj_boxed));

                let mut_ref = self.objects.last_mut().unwrap();

                OwnedObject::make_stack_object(mut_ref)
            }
        }
    }

    pub fn len(&self) -> usize {
        self.objects.len()
    }

    pub unsafe fn clear(&mut self) {
        self.objects.clear()
    }

    pub fn new_string(&mut self, s: &str) -> StackObject {
        self.store(s.to_string())
    }

    pub fn new_const_string(&mut self, s: &str) -> StackObject {
        //TODO: interning?
        self.store(_ConstHeapString(s.to_string()))
    }
}
