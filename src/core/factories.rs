//! Technology for registering factories of types and recognising
//! the types.

use std::rc::Rc;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt;
use std::ops::Deref;
use serde as sd;
use erased_serde as esd;
use serde_tagged::de::SeedFactory;
use serde_tagged::util::TagString;
use serde_tagged as sdt;
use serde_tagged::util::erased::SerializeErased;

/// Uniquely identify the type of an object
pub trait TypeId {
    fn type_id(&self) -> &'static str;
}

/// A registry of methods to deserialize objects given a tag to identify
/// the type of object, using TypeId.
pub struct Registry<V> {
    registry: HashMap<&'static str, V>
}

impl<V> Registry<V> {

    /// Creates an empty registry
    pub fn new() -> Registry<V> {
        Registry { registry: HashMap::new() }
    }

    /// Adds a creation method to the registry
    pub fn insert(&mut self, key: &'static str, value: V) {
        self.registry.insert(key, value);
    }

}

// Allow use of the registration as a seed factor, for deserialization
impl<'r, 'de, V, S> SeedFactory<'de, TagString<'de>> for &'r Registry<S>
where
    &'r S: sd::de::DeserializeSeed<'de, Value = V>
{
    type Value = V;
    type Seed = &'r S;

    fn seed<E>(self, tag: TagString<'de>) -> Result<Self::Seed, E>
    where
        E: sd::de::Error,
    {
        self.registry.get(tag.as_ref())
            .ok_or_else(|| sd::de::Error::custom(&format!("Unknown tag: {}", tag.as_ref())))
    }
}

/// A source of a registry
pub trait RegistrySource<T> {
    fn get_registry() -> &'static Registry<T>;
}

/// Our own reference counted type, so we can implement serialization and
/// deserialization.
pub struct Qrc<T: esd::Serialize + TypeId + Debug + ?Sized>(Rc<T>);

impl<T> Clone for Qrc<T>
where T: esd::Serialize + TypeId + Debug + ?Sized {
    fn clone(&self) -> Qrc<T> {
        Qrc::new(self.0.clone())
    }
}

impl<T> Deref for Qrc<T> 
where T: esd::Serialize + TypeId + Debug + ?Sized {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Qrc<T> 
where T: esd::Serialize + TypeId + Debug + ?Sized {
    pub fn new(stored: Rc<T>) -> Qrc<T> {
        Qrc(stored)
    }
}

impl<T> TypeId for Qrc<T> 
where T: esd::Serialize + TypeId + Debug + ?Sized {
    fn type_id(&self) -> &'static str {
        self.0.type_id()
    }
}

impl<T> Debug for Qrc<T> 
where T: esd::Serialize + TypeId + Debug + ?Sized {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> sd::Serialize for Qrc<T> 
where T: esd::Serialize + TypeId + Debug + ?Sized {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: sd::Serializer,
    {
        // As tag we simply use the ID provided by our `TypeId` trait.
        // To serialize our trait object value (without the tag) we actually
        // need to call `erased_serde::serialize`. We can do this by wrapping
        // the object in `SerializeErased`.
        // The `serialize` method of `serde_erased::ser::external` will apply
        // our type-id as tag to the trait-object.
        sdt::ser::external::serialize(serializer, self.type_id(), &SerializeErased(&*self.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;
    use serde;
    use serde_tagged;
    use serde_tagged::de::BoxFnSeed;
    //use core::dedup::{Dedup, DedupControl, Drc, InstanceId, FromId};
    //use std::cell::RefCell;
    use std::rc::Rc;

    // An example for de-/serialization of trait objects.
    // 
    // Serializes trait-objects by enhancing the stored information with a tag,
    // then later deserializes the stored tag, based on which a deserializer will
    // be chosen for the value.
    // 
    // The data-structures in this example are straightforward, meaning that
    // using an enum would probably make more sense here. However, enums can not
    // be extended, e.g. by a user of your library, thus sometimes trait-objects
    // are the only way.
 
    // Let's begin by defining some data-types.

    /// Our first type.
    #[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
    pub struct A {
        foo: String,
    }

    /// Our second type.
    #[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
    pub enum B {
        Str(String),
        Int(i64),
    }

    // You can use all de-/serializable data types in combination with this crate,
    // we however will limit ourselves to these two for this example.

    // We now need a way to identify our types.
    // For this, we create a new trait. This trait returns a tag that will later be
    // stored with our trait-object to describe its type.
    // In general, the only requirements on the tag are that it implements
    // `Serialize`. However since we are using the JSON format in combination with
    // external tagging (and JSON only allows strings for object-keys), the tag
    // must be a string.
    // See TypeId definition above

    // We also need to implement this trait for our types (and all types we want to
    // de-/serialize as the same trait objects).

    impl TypeId for A {
        fn type_id(&self) -> &'static str {
            "A"
        }
    }

    impl TypeId for B {
        fn type_id(&self) -> &'static str {
            "B"
        }
    }

    // Next we define the trait that we actually want to store as trait-object.
    // this trait should require our `TypeId` trait, as well as all other traits
    // that we want to be able to use on the trait object.

    // One trait that is required for serialization and must be present on the
    // trait-object to work is `erased_serde::Serialize`.
    // Note that we can not use `serde::Serialize` due to it containing a generic
    // method, however `erased_serde::Serialize` is automatically implemented for
    // all types implementing `serde::Serialize`, so the only thing we have to do
    // is add the trait bound here. No changes on our types are required.

    // FYI: This is required to enforce a fixed v-table layout, which is required
    //      to create a trait object from a set of traits.

    /// The trait we actually want to store as trait object.
    pub trait Stored: esd::Serialize + TypeId + Debug {}

    // In this case, we also want to automatically implement it for all types which
    // meet our requirements.
    impl<T> Stored for T
    where T: esd::Serialize + TypeId + Debug
    {}

    // Now we can implement `Serialize` and `Deserialize` for our trait objects.
    // In this example we use external tagging, but you could also use any other
    // strategy provided by this crate.

    // WARNING:
    // If we would serialize non-trait-objects (i.e. `Box<A>` or `Box<B>`), no
    // tag will be emitted, as the `Serialize` implementation of `Box<A>`
    // forwards to the `Serialize` implementation of `A`.
    // Thus you should make sure that you always serialize a trait-object when
    // you want to deserialize a trait object. To enforce this at compile time,
    // you could implement a custom wrapper type.

    impl<'a> serde::Serialize for Stored + 'a {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            // As tag we simply use the ID provided by our `TypeId` trait.
            // To serialize our trait object value (without the tag) we actually
            // need to call `erased_serde::serialize`. We can do this by wrapping
            // the object in `SerializeErased`.
            // The `serialize` method of `serde_erased::ser::external` will apply
            // our type-id as tag to the trait-object.
            serde_tagged::ser::external::serialize(serializer, self.type_id(), &SerializeErased(self))
        }
    }

    pub type RcStored = Qrc<Stored>;
    pub type TypeRegistry = Registry<BoxFnSeed<RcStored>>;

    /// Implement deserialization for subclasses of the type
    impl<'de> sd::Deserialize<'de> for Qrc<Stored> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: sd::Deserializer<'de>
        {
            sdt::de::external::deserialize(deserializer, get_registry())
        }
    }

    /// Return the type registry required for deserialization.
    pub fn get_registry() -> &'static TypeRegistry {
        lazy_static! {
            static ref REG: TypeRegistry = {
                let mut reg = TypeRegistry::new();
                reg.insert("A", BoxFnSeed::new(deserialize_erased::a));
                reg.insert("B", BoxFnSeed::new(deserialize_erased::b));
                reg
            };
        }
        &REG
    }

    // Due to the complexity of the function signature, we need to implement our
    // deserialization functions as actual functions. `rustc` will complain about
    // lifetime-issues (for the `Deserializer`) if we use closures.
    mod deserialize_erased {
        use super::*;
        use erased_serde::{Deserializer, Error};
        use serde::Deserialize;

        /// Deserialize a value of type `A` as trait-object.
        pub fn a<'de>(de: &mut Deserializer<'de>) -> Result<RcStored, Error> {
            Ok(RcStored::new(Rc::new(A::deserialize(de)?)))
        }

        /// Deserialize a value of type `B` as trait-object.
        pub fn b<'de>(de: &mut Deserializer<'de>) -> Result<RcStored, Error> {
            Ok(RcStored::new(Rc::new(B::deserialize(de)?)))
        }
    }

    #[test]
    fn serde_tagged_serialize() {

        // Let's begin by creating our test data ...
        let a : Rc<Stored> = Rc::new(A { foo: "bar".to_owned() });
        let b : Rc<Stored> = Rc::new(B::Str("Hello World".to_owned()));
        let c : Rc<Stored> = Rc::new(B::Int(42));

        // ... and then transform it to trait objects.
        // We use clone here so we can later assert that de-/serialization does not
        // change anything.
        let rc_a = RcStored::new(a.clone());
        let rc_b = RcStored::new(b.clone());
        let rc_c = RcStored::new(c.clone());
 
        // Now we can serialize our trait-objects.
        // Thanks to our `Serialize` implementation for trait objects this works
        // just like with any other type.
        let ser_a = serde_json::to_string_pretty(&rc_a).unwrap();
        let ser_b = serde_json::to_string_pretty(&rc_b).unwrap();
        let ser_c = serde_json::to_string_pretty(&rc_c).unwrap();

        // Again note the warning regarding serialization of non-trait-objects
        // above.

        // We specified external tagging, so we expect the following:
        assert_json_equal(&ser_a, r###"
        {
            "A": {
                "foo": "bar"
            }
        }
        "###);

        assert_json_equal(&ser_b, r###"
        {
            "B": {
                "Str": "Hello World"
            }
        }
        "###);

        assert_json_equal(&ser_c, r###"
        {
            "B": {
                "Int": 42
            }
        }
        "###);
    }

    #[test]
    fn serde_tagged_roundtrip() {

        // Let's begin by creating our test data ...
        let a : Rc<Stored> = Rc::new(A { foo: "bar".to_owned() });
        let b : Rc<Stored> = Rc::new(B::Str("Hello World".to_owned()));
        let c : Rc<Stored> = Rc::new(B::Int(42));

        // ... and then transform it to trait objects.
        // We use clone here so we can later assert that de-/serialization does not
        // change anything.
        let rc_a = RcStored::new(a.clone());
        let rc_b = RcStored::new(b.clone());
        let rc_c = RcStored::new(c.clone());
 
        // Now we can serialize our trait-objects.
        // Thanks to our `Serialize` implementation for trait objects this works
        // just like with any other type.
        let ser_a = serde_json::to_string_pretty(&rc_a).unwrap();
        let ser_b = serde_json::to_string_pretty(&rc_b).unwrap();
        let ser_c = serde_json::to_string_pretty(&rc_c).unwrap();

        // Again note the warning regarding serialization of non-trait-objects
        // above.

        // Now we let's deserialize our trait objects.
        // This works also just like any other type.
        let de_a: RcStored = serde_json::from_str(&ser_a).unwrap();
        let de_b: RcStored = serde_json::from_str(&ser_b).unwrap();
        let de_c: RcStored = serde_json::from_str(&ser_c).unwrap();

        assert_debug_eq(&rc_a, &de_a);
        assert_debug_eq(&rc_b, &de_b);
        assert_debug_eq(&rc_c, &de_c);
    }

/*
    // test deduplicated factories
    pub struct DrcStored(Drc<Stored, RcStored>);

    impl DrcStored {
        fn new(s: RcStored) { DrcStored(s) }
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
    pub struct C {
        id: &'static str,
        left: DrcStored,
        right: DrcStored
    }

    thread_local! {
        static DEDUP_STORED : RefCell<Dedup<Stored, DrcStored>> 
            = RefCell::new(Dedup::new(DedupControl::Inline, HashMap::new()));
    }

    impl InstanceId for A {
        fn id(&self) -> &str { &self.foo() }
    }

    impl InstanceId for B {
        fn id(&self) -> &str { "b" }
    }

    impl InstanceId for C {
        fn id(&self) -> &str { self.id }
    }

    impl FromId for DrcStored {
        fn from_id(id: &str) -> Option<Self> {
            DEDUP_STORED.with(|tls| tls.borrow().get(id).clone())
        }
    }

    impl sd::Serialize for DrcStored {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: sd::Serializer {
            self.serialize_with_dedup(serializer, &DEDUP_STORED)
        }
    }

    impl<'de> sd::Deserialize<'de> for DrcStored {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: sd::Deserializer<'de> {
            Self::deserialize_with_dedup(deserializer, &DEDUP_STORED)
        }
    }

    // this test just uses the default inline mode of dedup
    #[test]
    fn serde_tagged_dedup_inline_roundtrip() {

        // Let's begin by creating our test data ...
        let a = DrcStored::new(Rc::new(A { foo: "a".to_owned() }));
        let b = DrcStored::new(Rc::new(A { foo: "b".to_owned() }));
        let c = DrcStored::new(Rc::new(C { id: "c", left: a.clone(), right: b.clone() }));
        let d = DrcStored::new(Rc::new(C { id: "c", left: c.clone(), right: b.clone() }));

        // ... and then transform it to trait objects.
        // We use clone here so we can later assert that de-/serialization does not
        // change anything.
        let rc_a = a.clone();
        let rc_b = b.clone();
        let rc_c = c.clone();
        let rc_d = d.clone();
 
        // Now we can serialize our trait-objects.
        // Thanks to our `Serialize` implementation for trait objects this works
        // just like with any other type.
        let ser_a = serde_json::to_string_pretty(&rc_a).unwrap();
        let ser_b = serde_json::to_string_pretty(&rc_b).unwrap();
        let ser_c = serde_json::to_string_pretty(&rc_c).unwrap();
        let ser_d = serde_json::to_string_pretty(&rc_c).unwrap();

        // Again note the warning regarding serialization of non-trait-objects
        // above.

        // Now we let's deserialize our trait objects.
        // This works also just like any other type.
        let de_a: RcStored = serde_json::from_str(&ser_a).unwrap();
        let de_b: RcStored = serde_json::from_str(&ser_b).unwrap();
        let de_c: RcStored = serde_json::from_str(&ser_c).unwrap();
        let de_d: DrcStored = serde_json::from_str(&ser_d).unwrap();

        assert_debug_eq(&rc_a, &de_a);
        assert_debug_eq(&rc_b, &de_b);
        assert_debug_eq(&rc_c, &de_c);
        assert_debug_eq(&rc_d, &de_d);
    }
*/

    /// A helper function to assert that two strings contain the same JSON data.
    fn assert_json_equal(a: &str, b: &str) {
        let a: serde_json::Value = serde_json::from_str(a).unwrap();
        let b: serde_json::Value = serde_json::from_str(b).unwrap();
        assert_eq!(a, b);
    }

    /// A helper function to assert that the debug representations of two objects
    /// are the same
    fn assert_debug_eq(a: &RcStored, b: &RcStored) {
        let a_debug = format!("{:?}", a);
        let b_debug = format!("{:?}", b);
        assert_eq!(a_debug, b_debug);
    }
}