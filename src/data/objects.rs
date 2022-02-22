use crate::data::marked_counter::{MarkedCounter, UNMARKED_ONE};
use crate::Chunk;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

pub type Value = StackObject;

pub enum StackObject {
    Int(i64),
    Blank,
    Builtin(Box<str>),
    HeapObject(PrivatePtr<OwnedObject>),
}

pub struct OwnedObject {
    pub item: OwnedObjectItem,
    pub marker: MarkedCounter,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValueBox(pub StackObject);

impl Default for ValueBox {
    fn default() -> Self {
        ValueBox(StackObject::Int(0))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Closure {
    pub closed_values: Vec<StackObject>,
    pub underlying: StackObject, //actually Function
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Partial {
    pub target: Value,
    pub args: Vec<Value>,
}

impl Partial {
    pub fn new(target: Value, args: Vec<Value>) -> Self {
        Partial { target, args }
    }

    pub fn count_blanks(&self) -> usize {
        self.args
            .iter()
            .filter(|value| matches!(value, StackObject::Blank))
            .count()
    }

    pub fn substitute(&self, values: VVec) -> Self {
        let mut subs = values.into_iter();

        let args = self
            .args
            .iter()
            .map(|value| match value {
                Value::Blank => {
                    if let Some(substitute) = subs.next() {
                        substitute
                    } else {
                        StackObject::Blank
                    }
                }
                other => other.clone(),
            })
            .collect();

        Partial {
            target: self.target.clone(),
            args,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OwnedObjectItem {
    ConstantString(String),
    MutableString(String),
    Vector(VVec),
    Map(VMap),
    Box(ValueBox),
    Closure(Closure),
    Function(Chunk),
    Partial(Partial),
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

impl PtrWrapper for Partial {}

impl Display for StackObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackObject::Int(n) => {
                write!(f, "{}", n)
            }
            StackObject::HeapObject(ptr) => {
                write!(f, "{}", ptr.unwrap_ref())
            }
            StackObject::Builtin(s) => {
                write!(f, "builtin<{}>", s)
            }
            StackObject::Blank => {
                write!(f, "Blank")
            }
        }
    }
}

impl Debug for StackObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackObject::Int(i) => write!(f, "Int {}", i),
            StackObject::HeapObject(ptr) => {
                write!(f, "{:?}", ptr.unwrap_ref().item)
            }

            StackObject::Builtin(name) => write!(f, "Builtin[{}]", name),
            StackObject::Blank => {
                write!(f, "Blank")
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
            Int(..) => true,
            Builtin(..) | Blank => false,
            HeapObject(ptr) => ptr.unwrap_ref().can_hash(),
        }
    }

    pub fn as_heap_object(&self) -> Option<&mut OwnedObjectItem> {
        match self {
            StackObject::HeapObject(ptr) => Some(&mut ptr.unwrap_ref_mut().item),
            _ => None,
        }
    }

    pub fn unwrap_map(&self) -> Option<&mut VMap> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::Map(m)) => Some(m),
            _ => None,
        }
    }

    pub fn unwrap_vector(&self) -> Option<&mut VVec> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::Vector(v)) => Some(v),
            _ => None,
        }
    }

    pub fn unwrap_mutable_string(&self) -> Option<&mut String> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::MutableString(s)) => Some(s),
            _ => None,
        }
    }

    pub fn unwrap_const_string(&self) -> Option<&str> {
        //return &str because ConstString is guaranteed not to be mutable;
        match self.as_heap_object() {
            Some(OwnedObjectItem::ConstantString(s)) => Some(s),
            _ => None,
        }
    }

    pub fn unwrap_int(&self) -> Option<i64> {
        match self {
            StackObject::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub fn unwrap_partial(&self) -> Option<&mut Partial> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::Partial(p)) => Some(p),
            _ => None,
        }
    }

    pub fn unwrap_closure(&self) -> Option<&mut Closure> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::Closure(c)) => Some(c),
            _ => None,
        }
    }

    pub fn unwrap_box(&self) -> Option<&mut ValueBox> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::Box(b)) => Some(b),
            _ => None,
        }
    }

    pub(super) fn unwrap_traceable(&self) -> Option<&mut OwnedObject> {
        match self {
            StackObject::HeapObject(h) => Some(h.unwrap_ref_mut()),
            _ => None,
        }
    }

    pub fn unwrap_function(&self) -> Option<&mut Chunk> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::Function(chunk)) => Some(chunk),
            _ => None,
        }
    }

    pub fn unwrap_any_str(&self) -> Option<&str> {
        match self.as_heap_object() {
            Some(OwnedObjectItem::MutableString(m)) => Some(m.as_str()),
            Some(OwnedObjectItem::ConstantString(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            StackObject::Int(_) => "int".to_string(),
            StackObject::Builtin(_) => "Builtin".to_string(),
            StackObject::Blank => "Blank".to_string(),
            StackObject::HeapObject(ptr) => ptr.unwrap_ref().type_string(),
        }
    }

    fn cmp_any_strings(obj1: &StackObject, obj2: &StackObject) -> Option<Ordering> {
        match (obj1.unwrap_any_str(), obj2.unwrap_any_str()) {
            (Some(s1), Some(s2)) => Some(s1.cmp(s2)),
            _ => None,
        }
    }

    fn cmp_ints(obj1: &StackObject, obj2: &StackObject) -> Option<Ordering> {
        match (obj1.unwrap_int(), obj2.unwrap_int()) {
            (Some(n1), Some(n2)) => Some(n1.cmp(&n2)),
            _ => None,
        }
    }
}

impl PartialOrd for StackObject {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        StackObject::cmp_any_strings(self, other).or_else(|| StackObject::cmp_ints(self, other))
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
            (StackObject::HeapObject(ptr1), StackObject::HeapObject(ptr2)) => {
                std::ptr::eq(ptr1.unwrap(), ptr2.unwrap()) || ptr1.unwrap_ref() == ptr2.unwrap_ref()
            }
            (StackObject::Builtin(s1), StackObject::Builtin(s2)) => s1 == s2,
            (other1, other2) => panic!(
                "eq: same discriminant, not string, unhandled case {:?}",
                (other1, other2)
            ),
        }
    }
}

impl Eq for StackObject {}

impl Hash for StackObject {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StackObject::Int(i) => i.hash(state),
            StackObject::HeapObject(ptr) => ptr.unwrap_ref().hash(state),
            other => panic!("cannot hash {:?}", other),
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

    pub fn type_string(&self) -> String {
        match self.item {
            OwnedObjectItem::ConstantString(_) => "ConstantString".to_string(),
            OwnedObjectItem::MutableString(_) => "String".to_string(),
            OwnedObjectItem::Vector(_) => "Vector".to_string(),
            OwnedObjectItem::Map(_) => "Map".to_string(),
            OwnedObjectItem::Box(_) => "Box".to_string(),
            OwnedObjectItem::Closure(_) => "Closure".to_string(),
            OwnedObjectItem::Partial(_) => "Partial".to_string(),
            OwnedObjectItem::Function(..) => "Function".to_string(),
        }
    }
}

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

impl Debug for OwnedObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let content = match &self.item {
            OwnedObjectItem::ConstantString(s) => format!("ConstStr {:?} at {:p}", s, self),
            OwnedObjectItem::MutableString(s) => format!("MutStr \"{:?}\" at {:p}", s, self),
            OwnedObjectItem::Vector(v) => format!("{:?}", v),
            OwnedObjectItem::Map(m) => {
                format!("Map {:?} at {:p}", m, self)
            }
            OwnedObjectItem::Box(b) => format!("Box [{:?}] at {:p}", b.0, self),
            OwnedObjectItem::Closure(c) => format!(
                "Closure (over {}, values: {:?}) at {:p}",
                c.underlying, c.closed_values, self
            ),
            OwnedObjectItem::Partial(p) => format!(
                "Partial over {} with args {:?} at {:p}",
                p.target, p.args, self
            ),
            OwnedObjectItem::Function(chunk) => {
                format!(
                    "Function {} of {} args",
                    chunk.name.get_string().unwrap(),
                    chunk.arity
                )
            }
        };

        write!(f, "object [{}], RC={}", content, self.marker.counter())
    }
}

impl Display for OwnedObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.item {
            OwnedObjectItem::ConstantString(ptr) => write!(f, "{}", ptr),
            OwnedObjectItem::MutableString(ptr) => write!(f, "{}", ptr),
            OwnedObjectItem::Vector(ptr) => write!(f, "Vector {:?}", ptr),
            OwnedObjectItem::Map(ptr) => write!(f, "Map {:?}", ptr),
            OwnedObjectItem::Box(ptr) => write!(f, "box[{}]", ptr.0),
            OwnedObjectItem::Closure(closure) => {
                write!(
                    f,
                    "closure<{}, {}>",
                    closure.closed_values.len(),
                    closure.underlying
                )
            }
            OwnedObjectItem::Partial(ptr) => {
                write!(f, "partial[{}] over {}", ptr.args.len(), ptr.target)
            }
            OwnedObjectItem::Function(chunk) => {
                write!(
                    f,
                    "Function {} of {} args",
                    chunk.name.get_string().unwrap(),
                    chunk.arity
                )
            }
        }
    }
}

impl PartialEq for OwnedObject {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item
    }
}

impl Eq for OwnedObject {}

impl Clone for OwnedObject {
    fn clone(&self) -> Self {
        OwnedObject {
            marker: UNMARKED_ONE,
            item: self.item.clone(),
        }
    }
}
