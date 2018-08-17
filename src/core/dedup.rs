use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;
use std::thread::LocalKey;
use std::mem::swap;
use std::marker::PhantomData;
use std::marker::Sized;
use serde as sd;
use serde::ser::Error;
use serde::Deserialize;
use serde::Deserializer;
use serde::de::Visitor;

/// Technology to deduplicate directed acyclic graphs based on Rc pointers
/// during serialization and deserialization.
pub struct Dedup<T, R>
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
{
    control: DedupControl,
    map: HashMap<String, Drc<T, R>>
}

impl<T, R> Dedup<T, R>
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
{
    pub fn new(control: DedupControl, map: HashMap<String, Drc<T, R>>) -> Self {
        Dedup { control, map }
    }
   
    /// Run the supplied closure in the context of this Dedup state
    pub fn with<F, Q>(&mut self, tls_seed: &'static LocalKey<RefCell<Dedup<T, R>>>, f: F) -> Q 
    where F: FnOnce() -> Q {

        // store our state in a thread-local variable, as there is no other way of
        // passing state through the serialize stack
        tls_seed.with(|s| {
            swap(&mut *s.borrow_mut(), self);
        });

        let result = f();

        // by swapping in and out of our own member variables, we save the state of the
        // thread-local variable, as well as providing our own state to the serialize
        // functions
        tls_seed.with(|s| {
            swap(&mut *s.borrow_mut(), self);
        });

        result
    }

    pub fn control(&self) -> &DedupControl { &self.control }

    /// Tries to insert the element into the map. If it was inserted, returns None, otherwise
    /// returns the old value.
    pub fn insert(&mut self, value: Drc<T, R>) -> Option<Drc<T, R>> {
        self.map.insert(value.id().to_string(), value)
    }

    /// Finds an element given a string, or returns None if not found.
    pub fn get(&self, id: &str) -> Option<Drc<T, R>> {
        if let Some(element) = self.map.get(id) {
            Some(element.clone())
        } else {
            None
        }
    }

    /// Returns true if the map contains this key
    pub fn contains_key(&self, id: &str) -> bool {
        self.map.contains_key(id)
    }
}

/// What to do when we are serializing or deserializing and we meet a DAG node
#[derive(Clone, Copy, Debug)]
pub enum DedupControl {
    /// for deserializing or serializing with an expected set of components
    ErrorIfMissing,
    /// serialize with all components recursed inline
    Inline,
    /// when serializing, write once, then reuse
    WriteOnce
}

/// Objects that can be deduplicated have to have a unique instance id
pub trait InstanceId {
    fn id(&self) -> &str;
}

/// Wrap a type T in a container R so it can be used in a Drp<T, R>.
/// For example, R may be an Rc or Arc
pub trait Wrap<R> {
    fn wrap(self) -> R;
}

impl<T> Wrap<Rc<T>> for T {
    fn wrap(self) -> Rc<T> {
        Rc::new(self)
    }
}

impl<T> Wrap<Arc<T>> for T {
    fn wrap(self) -> Arc<T> {
        Arc::new(self)
    }
}

/// Edges in the graph must be contained in our own subclass of Rc, so we
/// can implement our serialize/deserialize methods on it.
pub struct Drc<T, R>(R)
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId;
    
impl<T, R> Drc<T, R> 
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
{
    pub fn new(value: R) -> Drc<T, R> {
        Drc(value)
    }

    pub fn content(&self) -> &R { &self.0 }

    pub fn serialize_with_dedup<S, F>(&self, serializer: S,
        tls_seed: &'static LocalKey<RefCell<Dedup<T, R>>>,
        serialize: F) -> Result<S::Ok, S::Error>
    where
        S: sd::Serializer,
        F: FnOnce(S) -> Result<S::Ok, S::Error> {

        let control = tls_seed.with(|tls| tls.borrow().control().clone());
        match control {
            // Inline serialization means we just recurse into the shared pointer. There is
            // no deduplication.
            DedupControl::Inline => serialize(serializer),

            // ErrorIfMissing means all subcomponents should be supplied in the map
            DedupControl::ErrorIfMissing => {
                let id = self.0.id();
                let has_key = tls_seed.with(|tls| tls.borrow().contains_key(id));
                if !has_key {
                    Err(S::Error::custom(&format!("Unknown id: {}", id)))
                } else {
                    serializer.serialize_str(id)
                }
            }

            // WriteOnce means we look for the component in the map, and write it and insert it
            // if it is not there
            DedupControl::WriteOnce => {
                let prev = tls_seed.with(|tls| tls.borrow_mut().insert(self.clone()));
                if let None = prev {
                    serialize(serializer)
                } else {
                    serializer.serialize_str(self.0.id())
                }
            }
        }
    }

    pub fn deserialize_with_dedup<'de, D, F>(deserializer: D,
        tls_seed: &'static LocalKey<RefCell<Dedup<T, R>>>,
        deserialize: F) -> Result<Self, D::Error>
    where 
        D: sd::Deserializer<'de>,
        F: FnOnce(D) -> Result<Self, D::Error>
    {
        let control = tls_seed.with(|tls| tls.borrow().control().clone());
        match control {
            // Inline deserialization means we assume the serial form contains the definition
            // of the data inline. Simply recurse into it.
            DedupControl::Inline => {
                deserialize(deserializer)
            },

            // ErrorIfMissing means all subcomponents should be supplied in the map. We should just
            // be looking for a string.
            DedupControl::ErrorIfMissing => {
                deserialize(deserializer)
            },

            // WriteOnce means we look for the component in the map, and write it and insert it
            // if it is not there
            DedupControl::WriteOnce => {
                let obj = deserialize(deserializer)?;
                tls_seed.with(|tls| tls.borrow_mut().insert(obj.clone()));
                Ok(obj)
            }
        }
    }
}

impl<T, R> Clone for Drc<T, R>
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
{
    fn clone(&self) -> Self {
        Drc::new(self.0.clone())
    }
}

impl<T, R> Deref for Drc<T, R> 
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
{
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T, R> Debug for Drc<T, R> 
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Interface that fetches an instance of an object given its ID.
pub trait FromId where Self: Sized {
    fn from_id(id: &str) -> Option<Self>;
}

// Code based on the example 'string_or_struct' given in the serde documentation
pub fn string_or_struct<'de, T, R, D>(deserializer: D) -> Result<Drc<T, R>, D::Error>
where
    T: InstanceId + Wrap<R> + Debug + ?Sized,
    for<'de2> T: sd::Deserialize<'de2>,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId,
    D: Deserializer<'de>,
{
    // This is a Visitor that forwards string types to T's `FromId` impl and
    // forwards map types to T's `Deserialize` impl. The `PhantomData` is to
    // keep the compiler from complaining about T being an unused generic type
    // parameter. We need T in order to know the Value type for the Visitor
    // impl.
    struct StringOrStruct<T, R>(PhantomData<fn() -> (T, R)>);

    impl<'de, T, R> Visitor<'de> for StringOrStruct<T, R>
    where
        T: InstanceId + Wrap<R> + Debug + ?Sized,
        for<'de2> T: sd::Deserialize<'de2>,
        R: Deref<Target = T> + Clone,
        Drc<T, R>: FromId,
    {
        type Value = Drc<T, R>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("string or map")
        }

        fn visit_str<E>(self, value: &str) -> Result<Drc<T, R>, E>
        where
            E: sd::de::Error,
        {
            if let Some(result) = FromId::from_id(value) {
                Ok(result)
            } else {
                Err(sd::de::Error::invalid_value(sd::de::Unexpected::Str(value), &self))
            }
        }

        fn visit_map<M>(self, visitor: M) -> Result<Drc<T, R>, M::Error>
        where
            M: sd::de::MapAccess<'de>,
        {
            // `MapAccessDeserializer` is a wrapper that turns a `MapAccess`
            // into a `Deserializer`, allowing it to be used as the input to T's
            // `Deserialize` implementation. T then deserializes itself using
            // the entries from the map visitor.
            let obj: T = Deserialize::deserialize(sd::de::value::MapAccessDeserializer::new(visitor))?;
            Ok(Drc::new(obj.wrap()))
        }
    }

    deserializer.deserialize_any(StringOrStruct(PhantomData))
}

/// Utility function to convert a slice of objects that support InstanceId into a
/// HashMap. We do this rather than using a HashSet and relying on the object's own
/// equality and hash methods, because we do not want to force every user of dedup
/// to apply equality and hash only to its id.
pub fn dedup_map_from_slice<T, R>(slice: &[Drc<T, R>]) -> HashMap<String, Drc<T, R>>
where
    T: InstanceId + Debug + ?Sized,
    R: Deref<Target = T> + Clone,
    Drc<T, R>: FromId
{
    let mut map = HashMap::new();
    for item in slice.iter() {
        let id = item.id();
        map.insert(id.to_string(), item.clone());
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;
    use serde::Deserialize;
    use serde::Serialize;

    /// an example node from a DAG (here a binary tree)
    #[derive(Serialize, Deserialize, Debug)]
    struct Node {
        id: String,
        data: i32,
        left: Option<Drc<Node, Rc<Node>>>,
        right: Option<Drc<Node, Rc<Node>>>
    }

    thread_local! {
        static DEDUP_NODE : RefCell<Dedup<Node, Rc<Node>>> 
            = RefCell::new(Dedup::new(DedupControl::Inline, HashMap::new()));
    }

    type DrcNode = Drc<Node, Rc<Node>>;

    impl InstanceId for Node {
        fn id(&self) -> &str { &self.id }
    }

    impl FromId for DrcNode {
        fn from_id(id: &str) -> Option<Self> {
            DEDUP_NODE.with(|tls| tls.borrow().get(id).clone())
        }
    }

    impl sd::Serialize for DrcNode {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: sd::Serializer {
            self.serialize_with_dedup(serializer, &DEDUP_NODE,
                |s| self.0.serialize(s))
        }
    }

    impl<'de> sd::Deserialize<'de> for DrcNode {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: sd::Deserializer<'de> {
            Self::deserialize_with_dedup(deserializer, &DEDUP_NODE, 
                |d| string_or_struct::<Node, Rc<Node>, D>(d))
        }
    }

    #[test]
    fn serde_dedup_inline() {

        test_dedup(DedupControl::Inline, HashMap::new(), r###"{
  "id": "e",
  "data": 4,
  "left": {
    "id": "c",
    "data": 2,
    "left": {
      "id": "a",
      "data": 0,
      "left": null,
      "right": null
    },
    "right": {
      "id": "b",
      "data": 1,
      "left": null,
      "right": null
    }
  },
  "right": {
    "id": "d",
    "data": 3,
    "left": {
      "id": "a",
      "data": 0,
      "left": null,
      "right": null
    },
    "right": {
      "id": "b",
      "data": 1,
      "left": null,
      "right": null
    }
  }
}"###);
    }

    #[test]
    fn serde_dedup_error_if_missing() {

        let a = DrcNode::new(Rc::new(Node { id: "a".to_string(), data: 0, left: None, right: None }));
        let b = DrcNode::new(Rc::new(Node { id: "b".to_string(), data: 1, left: None, right: None }));
        let c = DrcNode::new(Rc::new(Node { id: "c".to_string(), data: 2, left: Some(a.clone()), right: Some(b.clone()) }));
        let d = DrcNode::new(Rc::new(Node { id: "d".to_string(), data: 3, left: Some(a.clone()), right: Some(b.clone()) }));
        
        let mut map = HashMap::new();
        map.insert("a".to_string(), a);
        map.insert("b".to_string(), b);
        map.insert("c".to_string(), c);
        map.insert("d".to_string(), d);

        test_dedup(DedupControl::ErrorIfMissing, map, r###"{
  "id": "e",
  "data": 4,
  "left": "c",
  "right": "d"
}"###);
    }

    #[test]
    fn serde_dedup_write_once() {
        test_dedup(DedupControl::WriteOnce, HashMap::new(), r###"{
  "id": "e",
  "data": 4,
  "left": {
    "id": "c",
    "data": 2,
    "left": {
      "id": "a",
      "data": 0,
      "left": null,
      "right": null
    },
    "right": {
      "id": "b",
      "data": 1,
      "left": null,
      "right": null
    }
  },
  "right": {
    "id": "d",
    "data": 3,
    "left": "a",
    "right": "b"
  }
}"###);
    }

    fn test_dedup(control: DedupControl, map: HashMap<String, DrcNode>, expected: &str) {

        // create a sample graph
        let a = DrcNode::new(Rc::new(Node { id: "a".to_string(), data: 0, left: None, right: None }));
        let b = DrcNode::new(Rc::new(Node { id: "b".to_string(), data: 1, left: None, right: None }));
        let c = DrcNode::new(Rc::new(Node { id: "c".to_string(), data: 2, left: Some(a.clone()), right: Some(b.clone()) }));
        let d = DrcNode::new(Rc::new(Node { id: "d".to_string(), data: 3, left: Some(a.clone()), right: Some(b.clone()) }));
        let e = Node { id: "e".to_string(), data: 4, left: Some(c.clone()), right: Some(d.clone()) };

        // write it out and read it back in
        let mut buffer = Vec::new();
        {
            let mut serializer = serde_json::Serializer::pretty(&mut buffer);
            let mut seed = Dedup::<Node, Rc<Node>>::new(control.clone(), map.clone());
            seed.with(&DEDUP_NODE, || e.serialize(&mut serializer)).unwrap();
        }

        let json = String::from_utf8(buffer.clone()).unwrap();
        print!("{}", json);
        assert_eq!(json, expected);

        let deserialized = {
            let mut deserializer = serde_json::Deserializer::from_slice(&buffer);
            let mut seed = Dedup::<Node, Rc<Node>>::new(control.clone(), map.clone());
            seed.with(&DEDUP_NODE, || DrcNode::deserialize(&mut deserializer)).unwrap()
        };

        let e_debug = format!("{:?}", e);
        let deserialized_debug = format!("{:?}", deserialized);
        assert_eq!(deserialized_debug, e_debug);
    }
}