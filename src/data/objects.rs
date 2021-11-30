use std::cmp::Ordering;
use crate::data::marked_counter::MarkedCounter;
use crate::data::objects::StackObject::Function;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

pub type Value = StackObject;

#[derive(Debug)]
pub enum StackObject {
    Int(i64),
    Function { chunk_id: usize },
    Map(PrivatePtr<OwnedObject>, PrivatePtr<VMap>),
    Vector(PrivatePtr<OwnedObject>, PrivatePtr<VVec>),
    MutableString(PrivatePtr<OwnedObject>, PrivatePtr<String>),
    ConstantString(PrivatePtr<OwnedObject>, PrivatePtr<String>),
    Box(PrivatePtr<OwnedObject>, PrivatePtr<ValueBox>),
    Closure(PrivatePtr<OwnedObject>, PrivatePtr<Closure>),
    Builtin(&'static str),
}

#[derive(Clone, Debug)]
pub struct OwnedObject {
    pub item: OwnedObjectItem,
    pub marker: MarkedCounter,
}

#[derive(Clone, Debug)]
pub struct ValueBox(pub StackObject);

impl Default for ValueBox {
    fn default() -> Self {
        ValueBox(StackObject::Int(0))
    }
}

#[derive(Clone, Debug)]
pub struct Closure {
    pub closed_values: Vec<StackObject>,
    pub chunk_id: usize,
}

#[derive(Clone, Debug)]
pub enum OwnedObjectItem {
    ConstantString(String),
    MutableString(String),
    Vector(VVec),
    Map(VMap),
    Box(ValueBox),
    Closure(Closure),
}

pub type VVec = Vec<StackObject>;
pub type VMap = HashMap<StackObject, StackObject>;

#[derive(Debug)]
pub struct PrivatePtr<T> {
    ptr: NonNull<T>,
}

impl<T> PrivatePtr<T> {
    #[inline(always)]
    pub(self) fn unwrap(&self) -> *mut T {
        self.ptr.as_ptr()
    }
    pub fn unwrap_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn unwrap_ref_mut(&self) -> &mut T {
        unsafe {
            self.ptr
                .as_ptr()
                .as_mut()
                .expect("null ptr in unwrap_ref_mut")
        }
    }
}

impl<T> Clone for PrivatePtr<T> {
    fn clone(&self) -> Self {
        PrivatePtr { ptr: self.ptr }
    }
}

impl<T> Copy for PrivatePtr<T> {}

pub(super) trait PtrWrapper
where
    Self: Sized,
{
    fn wrap_private(&mut self) -> PrivatePtr<Self> {
        PrivatePtr {
            ptr: NonNull::from(self),
        }
    }
}

impl PtrWrapper for OwnedObject {}

impl PtrWrapper for VMap {}

impl PtrWrapper for VVec {}

impl PtrWrapper for String {}

impl PtrWrapper for ValueBox {}

impl PtrWrapper for Closure {}

impl Display for StackObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackObject::Int(n) => {
                write!(f, "{}", n)
            }
            StackObject::Function { chunk_id, .. } => {
                write!(f, "function<{}>", chunk_id)
            }
            StackObject::Map(_, ptr) => write!(f, "Map {:?}", ptr.unwrap_ref()),

            StackObject::Vector(_, ptr) => write!(f, "Vector {:?}", ptr.unwrap_ref()),

            StackObject::MutableString(_, ptr) => write!(f, "{}", ptr.unwrap_ref()),

            StackObject::ConstantString(_, ptr) => write!(f, "{}", ptr.unwrap_ref()),
            StackObject::Box(_, ptr) => write!(f, "box[{}]", ptr.unwrap_ref().0),
            StackObject::Closure(_, ptr) => {
                let closure = ptr.unwrap_ref();
                write!(
                    f,
                    "closure<{}, {}>",
                    closure.closed_values.len(),
                    closure.chunk_id
                )
            }
            StackObject::Builtin(s) => {
                write!(f, "builtin<{}>", s)
            }
        }
    }
}

#[allow(dead_code)]
impl StackObject {
    pub fn wrap_from_int(x: i64) -> StackObject {
        StackObject::Int(x)
    }

    pub fn can_hash(&self) -> bool {
        use StackObject::*;
        match self {
            Int(..) | MutableString(..) | ConstantString(..) => true,
            Box(_gc_ptr, obj_ptr) => {
                let object = obj_ptr.unwrap_ref();
                object.0.can_hash()
            }

            Map(..) | Vector(..) | Function { .. } | Closure(..) | Builtin(..) => false,
        }
    }

    pub fn unwrap_map(&mut self) -> Option<&mut VMap> {
        match self {
            StackObject::Map(_, ptr) => Some(ptr.unwrap_ref_mut()),
            _ => None,
        }
    }

    pub fn unwrap_vector(&mut self) -> Option<&mut VVec> {
        match self {
            StackObject::Vector(_, ptr) => Some(ptr.unwrap_ref_mut()),
            _ => None,
        }
    }

    pub fn unwrap_mutable_string(&mut self) -> Option<&mut String> {
        match self {
            StackObject::MutableString(_, ptr) => Some(ptr.unwrap_ref_mut()),
            _ => None,
        }
    }

    pub fn unwrap_const_string(&mut self) -> Option<&str> {
        //return &str because ConstString is guaranteed not to be mutable;
        match self {
            StackObject::ConstantString(_, ptr) => Some(ptr.unwrap_ref().as_str()),
            _ => None,
        }
    }

    pub fn unwrap_int(&self) -> Option<i64> {
        match self {
            StackObject::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub(super) fn unwrap_traceable(&self) -> Option<&mut OwnedObject> {
        match self {
            StackObject::MutableString(trace_ptr, _)
            | StackObject::Vector(trace_ptr, _)
            | StackObject::Map(trace_ptr, _)
            | StackObject::Box(trace_ptr, _)
            | StackObject::Closure(trace_ptr, _)
            | StackObject::ConstantString(trace_ptr, _) => Some(trace_ptr.unwrap_ref_mut()),

            StackObject::Int(_) => None,
            StackObject::Function { .. } => None,
            StackObject::Builtin(..) => None,
        }
    }

    pub fn unwrap_any_str(&self) -> Option<&str> {
        match self {
            StackObject::MutableString(_, ptr) => Some(ptr.unwrap_ref().as_str()),
            StackObject::ConstantString(_, ptr) => Some(ptr.unwrap_ref().as_str()),
            _ => None,
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            StackObject::Int(_) => "int".to_string(),
            StackObject::Function { .. } => "function".to_string(),
            StackObject::Map(_, _) => "Map".to_string(),
            StackObject::Vector(_, _) => "Vector".to_string(),
            StackObject::MutableString(_, _) => "String".to_string(),
            StackObject::ConstantString(_, _) => "ConstantString".to_string(),
            StackObject::Box(_, _) => "Box".to_string(),
            StackObject::Closure(_, _) => "Closure".to_string(),
            StackObject::Builtin(_) => "Builtin".to_string(),
        }
    }
    
    fn cmp_any_strings(obj1: &StackObject, obj2: &StackObject) -> Option<Ordering> {
        match (obj1.unwrap_any_str(), obj2.unwrap_any_str()) {
            (Some(s1), Some(s2)) => Some(s1.cmp(s2)),
            _ => None
        }
    }
    
    fn cmp_ints(obj1: &StackObject, obj2: &StackObject) -> Option<Ordering> {
        match (obj1.unwrap_int(), obj2.unwrap_int()) {
            (Some(n1), Some(n2)) => Some(n1.cmp(&n2)),
            _ => None
        }
    }
}

impl PartialOrd for StackObject {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        StackObject::cmp_any_strings(self, other)
            .or_else(|| {StackObject::cmp_ints(self, other)})
    }
}

impl PartialEq for StackObject {
    fn eq(&self, other: &Self) -> bool {
        //string equality - MutString, ShortString, (maybe ConstString)
        if let (Some(s1), Some(s2)) = (self.unwrap_any_str(), other.unwrap_any_str()) {
            return s1 == s2;
        };

        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }

        match (self, other) {
            (StackObject::Int(a), StackObject::Int(b)) => a == b,

            (StackObject::Map(_, ptr1), StackObject::Map(_, ptr2)) => {
                std::ptr::eq(ptr1.unwrap(), ptr2.unwrap()) || ptr1.unwrap_ref() == ptr2.unwrap_ref()
            }

            (StackObject::Vector(_, ptr1), StackObject::Vector(_, ptr2)) => {
                std::ptr::eq(ptr1.unwrap(), ptr2.unwrap()) || ptr1.unwrap_ref() == ptr2.unwrap_ref()
            }
            (StackObject::Builtin(s1), StackObject::Builtin(s2)) => s1 == s2,
            _ => panic!("eq: same discriminant, not string, unhandled case"),
        }
    }
}

impl Eq for StackObject {}

impl Hash for StackObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StackObject::Int(i) => i.hash(state),
            StackObject::MutableString(_, ptr) => ptr.unwrap_ref().hash(state),

            StackObject::ConstantString(_, ptr) => ptr.unwrap_ref().hash(state),

            StackObject::Vector(..) | StackObject::Map(..) | Function { .. } => {
                panic!("hash on unhashable object")
            }
            StackObject::Box(..) => panic!("cannot hash box"),
            StackObject::Closure(..) => panic!("cannot hash closure"),
            StackObject::Builtin(..) => panic!("cannot hash builtin"),
        }
    }
}

#[allow(dead_code)]
impl OwnedObject {
    pub fn can_hash(&self) -> bool {
        matches!(
            &self.item,
            OwnedObjectItem::MutableString(..) | OwnedObjectItem::ConstantString(..)
        )
    }

    fn unwrap_map(&mut self) -> Option<&mut VMap> {
        match &mut self.item {
            OwnedObjectItem::Map(object) => Some(object),
            _ => None,
        }
    }

    fn unwrap_vector(&mut self) -> Option<&mut VVec> {
        match &mut self.item {
            OwnedObjectItem::Vector(object) => Some(object),
            _ => None,
        }
    }

    fn unwrap_string(&mut self) -> Option<&mut String> {
        match &mut self.item {
            OwnedObjectItem::MutableString(object) => Some(object),
            _ => None,
        }
    }
}

impl PartialEq for OwnedObject {
    fn eq(&self, other: &Self) -> bool {
        if std::mem::discriminant(&self.item) != std::mem::discriminant(&other.item) {
            return false;
        }

        match (&self.item, &other.item) {
            (OwnedObjectItem::Vector(object1), OwnedObjectItem::Vector(object2)) => {
                object1 == object2
            }

            (OwnedObjectItem::Map(object1), OwnedObjectItem::Map(object2)) => object1 == object2,
            (OwnedObjectItem::MutableString(obj1), OwnedObjectItem::MutableString(obj2)) => {
                obj1 == obj2
            }
            _ => panic!(),
        }
    }
}

impl Eq for OwnedObject {}

impl Hash for OwnedObject {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        match &self.item {
            OwnedObjectItem::MutableString(s) => s.hash(_state),

            OwnedObjectItem::ConstantString(s) => s.hash(_state),

            _ => panic!("unhashable type {:?}", self),
        }
    }
}

impl Deref for OwnedObject {
    type Target = OwnedObjectItem;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl DerefMut for OwnedObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}
