use crate::data::objects::StackObject::Function;
use crate::execution::chunk::Chunk;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

pub type Value = StackObject;

#[derive(Copy, Clone, Debug)]
pub enum StackObject {
    Int(i64),
    Function { chunk_id: usize },
    Map(PrivatePtr<OwnedObject>, PrivatePtr<VMap>),
    Vector(PrivatePtr<OwnedObject>, PrivatePtr<VVec>),
    MutableString(PrivatePtr<OwnedObject>, PrivatePtr<String>),
    ConstantString(PrivatePtr<OwnedObject>, PrivatePtr<String>),
}

#[derive(Clone, Debug)]
pub struct OwnedObject {
    pub item: OwnedObjectItem,
    pub marker: bool,
}

#[derive(Clone, Debug)]
pub enum OwnedObjectItem {
    ConstantString(String),
    MutableString(String),
    Vector(VVec),
    Map(VMap),
}

pub type VVec = Vec<StackObject>;
pub type VMap = HashMap<StackObject, StackObject>;

#[derive(Debug)]
pub struct PrivatePtr<T> {
    ptr: *mut T,
}

impl<T> PrivatePtr<T> {
    #[inline(always)]
    pub(self) fn unwrap(&self) -> *mut T {
        self.ptr
    }
    pub(super) fn unwrap_ref(&self) -> Option<&T> {
        unsafe { self.ptr.as_ref() }
    }
    pub(super) fn unwrap_ref_mut(&self) -> Option<&mut T> {
        unsafe { self.ptr.as_mut() }
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
            ptr: (self as *mut Self),
        }
    }
}

impl PtrWrapper for OwnedObject {}

impl PtrWrapper for VMap {}

impl PtrWrapper for VVec {}

impl PtrWrapper for String {}

impl Display for StackObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackObject::Int(n) => {
                write!(f, "{}", n)
            }
            StackObject::Function { .. } => {
                write!(f, "{}", "function")
            }
            StackObject::Map(_, ptr) => write!(
                f,
                "{}",
                ptr.unwrap_ref()
                    .map(|o| format!("Map {:?}", o))
                    .unwrap_or("null".to_string())
            ),

            StackObject::Vector(_, ptr) => write!(
                f,
                "{}",
                ptr.unwrap_ref()
                    .map(|o| format!("Vector {:?}", o))
                    .unwrap_or("null".to_string()),
            ),

            StackObject::MutableString(_, ptr) => write!(
                f,
                "{}",
                ptr.unwrap_ref()
                    .map(|o| format!("{}", o))
                    .unwrap_or("null".to_string()),
            ),

            StackObject::ConstantString(_, ptr) => write!(
                f,
                "{}",
                ptr.unwrap_ref()
                    .map(|o| format!("{}", o))
                    .unwrap_or("null".to_string()),
            ),
        }
    }
}

impl StackObject {
    pub fn wrap_from_int(x: i64) -> StackObject {
        StackObject::Int(x)
    }

    pub fn can_hash(&self) -> bool {
        use StackObject::*;
        match self {
            Int(..) | MutableString(..) | ConstantString(..) => true,

            Map(..) | Vector(..) | Function { .. } => false,
        }
    }

    pub fn unwrap_map(&mut self) -> Option<&mut VMap> {
        match self {
            StackObject::Map(_, ptr) => ptr.unwrap_ref_mut(),
            _ => None,
        }
    }

    pub fn unwrap_vector(&mut self) -> Option<&mut VVec> {
        match self {
            StackObject::Vector(_, ptr) => ptr.unwrap_ref_mut(),
            _ => None,
        }
    }

    pub fn unwrap_mutable_string(&mut self) -> Option<&mut String> {
        match self {
            StackObject::MutableString(_, ptr) => ptr.unwrap_ref_mut(),
            _ => None,
        }
    }

    pub fn unwrap_const_string(&mut self) -> Option<&str> {
        //return &str because ConstString is guaranteed not to be mutable;
        match self {
            StackObject::ConstantString(_, ptr) => ptr.unwrap_ref().map(|obj| obj.as_str()),
            _ => None,
        }
    }

    pub fn unwrap_int(self) -> Option<i64> {
        match self {
            StackObject::Int(n) => Some(n),
            _ => None,
        }
    }

    pub(super) fn unwrap_traceable(&self) -> Option<&mut OwnedObject> {
        match self {
            StackObject::MutableString(trace_ptr, _)
            | StackObject::Vector(trace_ptr, _)
            | StackObject::Map(trace_ptr, _)
            | StackObject::ConstantString(trace_ptr, _) => trace_ptr.unwrap_ref_mut(),

            StackObject::Int(_) => None,
            StackObject::Function { .. } => None,
        }
    }

    pub fn unwrap_any_str(&self) -> Option<&str> {
        match self {
            StackObject::MutableString(_, ptr) => ptr.unwrap_ref().map(|o| o.as_str()),
            StackObject::ConstantString(_, ptr) => ptr.unwrap_ref().map(|o| o.as_str()),
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
        }
    }
}

impl PartialEq for StackObject {
    fn eq(&self, other: &Self) -> bool {
        //string equality - MutString, ShortString, (maybe ConstString)
        match (self.unwrap_any_str(), other.unwrap_any_str()) {
            (Some(s1), Some(s2)) => {
                return s1 == s2;
            }
            _ => {}
        };

        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }

        match (self, other) {
            (StackObject::Int(a), StackObject::Int(b)) => a == b,

            (StackObject::Map(_, ptr1), StackObject::Map(_, ptr2)) => {
                std::ptr::eq(ptr1.unwrap(), ptr2.unwrap())
                    || match (ptr1.unwrap_ref(), ptr2.unwrap_ref()) {
                        (None, None) => true, //null pointers are considered equal
                        // (this should never execute normally)
                        (Some(r1), Some(r2)) => r1 == r2,
                        _ => false,
                    }
            }

            (StackObject::Vector(_, ptr1), StackObject::Vector(_, ptr2)) => {
                std::ptr::eq(ptr1.unwrap(), ptr2.unwrap())
                    || match (ptr1.unwrap_ref(), ptr2.unwrap_ref()) {
                        (None, None) => true, //null pointers are considered equal
                        // (this should never execute normally)
                        (Some(r1), Some(r2)) => r1 == r2,
                        _ => false,
                    }
            }
            _ => panic!("eq: same discriminant, not string, unhandled case"),
        }
    }
}

impl Eq for StackObject {}

impl Hash for StackObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StackObject::Int(i) => i.hash(state),
            StackObject::MutableString(_, ptr) => ptr.unwrap_ref().unwrap().hash(state),

            StackObject::ConstantString(_, ptr) => ptr
                .unwrap_ref()
                .expect("null reference in const string")
                .hash(state),

            StackObject::Vector(..) | StackObject::Map(..) | Function { .. } => {
                panic!("hash on unhashable object")
            }
        }
    }
}

impl OwnedObject {
    pub fn can_hash(&self) -> bool {
        match &self.item {
            OwnedObjectItem::MutableString(..) | OwnedObjectItem::ConstantString(..) => true,

            _ => false,
        }
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
