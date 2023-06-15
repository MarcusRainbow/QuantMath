//! Technology for registering factories of types and recognising
//! the types.

use erased_serde as esd;
use serde as sd;
use serde_tagged as sdt;
use serde_tagged::de::SeedFactory;
use serde_tagged::util::erased::SerializeErased;
use serde_tagged::util::TagString;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::Arc;

/// Uniquely identify the type of an object
pub trait TypeId {
    fn get_type_id(&self) -> &'static str;
}

/// A registry of methods to deserialize objects given a tag to identify
/// the type of object, using TypeId.
pub struct Registry<V> {
    registry: HashMap<&'static str, V>,
}

impl<V> Registry<V> {
    /// Creates an empty registry
    pub fn new() -> Registry<V> {
        Registry {
            registry: HashMap::new(),
        }
    }

    /// Adds a creation method to the registry
    pub fn insert(&mut self, key: &'static str, value: V) {
        self.registry.insert(key, value);
    }
}

// Allow use of the registration as a seed factor, for deserialization
impl<'r, 'de, V, S> SeedFactory<'de, TagString<'de>> for &'r Registry<S>
where
    &'r S: sd::de::DeserializeSeed<'de, Value = V>,
{
    type Value = V;
    type Seed = &'r S;

    fn seed<E>(self, tag: TagString<'de>) -> Result<Self::Seed, E>
    where
        E: sd::de::Error,
    {
        self.registry
            .get(tag.as_ref())
            .ok_or_else(|| sd::de::Error::custom(&format!("Unknown tag: {}", tag.as_ref())))
    }
}

/// A source of a registry
pub trait RegistrySource<T> {
    fn get_registry() -> &'static Registry<T>;
}

/// Our own reference counted type, so we can implement serialization and
/// deserialization.
pub struct Qrc<T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized>(Arc<T>);

impl<T> Clone for Qrc<T>
where
    T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized,
{
    fn clone(&self) -> Qrc<T> {
        Qrc::new(self.0.clone())
    }
}

impl<T> Deref for Qrc<T>
where
    T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized,
{
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Qrc<T>
where
    T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized,
{
    pub fn new(stored: Arc<T>) -> Qrc<T> {
        Qrc(stored)
    }
}

impl<T> TypeId for Qrc<T>
where
    T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized,
{
    fn get_type_id(&self) -> &'static str {
        self.0.get_type_id()
    }
}

impl<T> Debug for Qrc<T>
where
    T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> sd::Serialize for Qrc<T>
where
    T: esd::Serialize + TypeId + Debug + Send + Sync + ?Sized,
{
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
        sdt::ser::external::serialize(serializer, self.get_type_id(), &SerializeErased(&*self.0))
    }
}

/// Our own box type, so we can implement serialization and
/// deserialization.
pub struct Qbox<T: esd::Serialize + TypeId + Debug + ?Sized>(Box<T>);

impl<T> Deref for Qbox<T>
where
    T: esd::Serialize + TypeId + Debug + ?Sized,
{
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Qbox<T>
where
    T: esd::Serialize + TypeId + Debug + ?Sized,
{
    pub fn new(stored: Box<T>) -> Qbox<T> {
        Qbox(stored)
    }
}

impl<T> TypeId for Qbox<T>
where
    T: esd::Serialize + TypeId + Debug + ?Sized,
{
    fn get_type_id(&self) -> &'static str {
        self.0.get_type_id()
    }
}

impl<T> Debug for Qbox<T>
where
    T: esd::Serialize + TypeId + Debug + ?Sized,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> sd::Serialize for Qbox<T>
where
    T: esd::Serialize + TypeId + Debug + ?Sized,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: sd::Serializer,
    {
        sdt::ser::external::serialize(serializer, self.get_type_id(), &SerializeErased(&*self.0))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use core::dedup::{Dedup, DedupControl, Drc, FromId, InstanceId};
    use serde;
    use serde::Deserialize;
    use serde::Serialize;
    use serde_json;
    use serde_tagged;
    use serde_tagged::de::BoxFnSeed;
    use std::cell::RefCell;
    use std::sync::Arc;

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
        fn get_type_id(&self) -> &'static str {
            "A"
        }
    }

    impl TypeId for B {
        fn get_type_id(&self) -> &'static str {
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

    // The subtrait InstanceId is required for deduplication only.

    /// The trait we actually want to store as trait object.
    pub trait Stored: esd::Serialize + TypeId + InstanceId + Sync + Send + Debug {}

    // In this case, we also want to automatically implement it for all types which
    // meet our requirements.
    impl<T> Stored for T where T: esd::Serialize + TypeId + InstanceId + Sync + Send + Debug {}

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
            serde_tagged::ser::external::serialize(
                serializer,
                self.get_type_id(),
                &SerializeErased(self),
            )
        }
    }

    pub type RcStored = Qrc<Stored>;
    pub type TypeRegistry = Registry<BoxFnSeed<RcStored>>;

    /// Implement deserialization for subclasses of the type
    impl<'de> sd::Deserialize<'de> for Qrc<Stored> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: sd::Deserializer<'de>,
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
                reg.insert("C", BoxFnSeed::new(deserialize_erased::c));
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
            Ok(RcStored::new(Arc::new(A::deserialize(de)?)))
        }

        /// Deserialize a value of type `B` as trait-object.
        pub fn b<'de>(de: &mut Deserializer<'de>) -> Result<RcStored, Error> {
            Ok(RcStored::new(Arc::new(B::deserialize(de)?)))
        }

        /// Deserialize a value of type `B` as trait-object.
        pub fn c<'de>(de: &mut Deserializer<'de>) -> Result<RcStored, Error> {
            Ok(RcStored::new(Arc::new(C::deserialize(de)?)))
        }
    }

    #[test]
    fn serde_tagged_serialize() {
        // Let's begin by creating our test data ...
        let a: Arc<Stored> = Arc::new(A {
            foo: "bar".to_owned(),
        });
        let b: Arc<Stored> = Arc::new(B::Str("Hello World".to_owned()));
        let c: Arc<Stored> = Arc::new(B::Int(42));

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
        assert_json_equal(
            &ser_a,
            r###"
        {
            "A": {
                "foo": "bar"
            }
        }
        "###,
        );

        assert_json_equal(
            &ser_b,
            r###"
        {
            "B": {
                "Str": "Hello World"
            }
        }
        "###,
        );

        assert_json_equal(
            &ser_c,
            r###"
        {
            "B": {
                "Int": 42
            }
        }
        "###,
        );
    }

    #[test]
    fn serde_tagged_roundtrip() {
        // Let's begin by creating our test data ...
        let a: Arc<Stored> = Arc::new(A {
            foo: "bar".to_owned(),
        });
        let b: Arc<Stored> = Arc::new(B::Str("Hello World".to_owned()));
        let c: Arc<Stored> = Arc::new(B::Int(42));

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

    // test deduplicated factories
    pub type DrcStored = Drc<Stored, RcStored>;

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct C {
        id: String,
        left: DrcStored,
        right: DrcStored,
    }

    thread_local! {
        static DEDUP_STORED : RefCell<Dedup<Stored, RcStored>>
            = RefCell::new(Dedup::new(DedupControl::Inline, HashMap::new()));
    }

    impl InstanceId for A {
        fn id(&self) -> &str {
            &self.foo
        }
    }

    impl InstanceId for B {
        fn id(&self) -> &str {
            "b"
        }
    }

    impl TypeId for C {
        fn get_type_id(&self) -> &'static str {
            "C"
        }
    }

    impl InstanceId for C {
        fn id(&self) -> &str {
            &self.id
        }
    }

    impl FromId for DrcStored {
        fn from_id(id: &str) -> Option<Self> {
            DEDUP_STORED.with(|tls| tls.borrow().get(id).clone())
        }
    }

    impl sd::Serialize for DrcStored {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: sd::Serializer,
        {
            self.serialize_with_dedup(serializer, &DEDUP_STORED, |s| {
                self.deref().deref().serialize(s)
            })
        }
    }

    // Sadly, I don't think there is an easy way to make this generic. Part of the problem is the
    // interface to visitor, which parameterises the types for Error and Visitor. It is therefore
    // hard to implement this in a functor or passed-in interface that does not simply reproduce
    // visitor (as here).
    pub fn string_or_struct_polymorphic<'de, D>(deserializer: D) -> Result<DrcStored, D::Error>
    where
        D: sd::de::Deserializer<'de>,
    {
        // This is a Visitor that forwards string types to T's `FromId` impl and
        // forwards map types to T's `Deserialize` impl. The `PhantomData` is to
        // keep the compiler from complaining about T being an unused generic type
        // parameter. We need T in order to know the Value type for the Visitor
        // impl.
        struct StringOrStruct();

        impl<'de> sd::de::Visitor<'de> for StringOrStruct {
            type Value = DrcStored;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("string or map")
            }

            fn visit_str<E>(self, value: &str) -> Result<DrcStored, E>
            where
                E: sd::de::Error,
            {
                if let Some(result) = FromId::from_id(value) {
                    Ok(result)
                } else {
                    Err(sd::de::Error::invalid_value(
                        sd::de::Unexpected::Str(value),
                        &self,
                    ))
                }
            }

            fn visit_map<M>(self, visitor: M) -> Result<DrcStored, M::Error>
            where
                M: sd::de::MapAccess<'de>,
            {
                let obj: RcStored = sd::de::Deserialize::deserialize(
                    sd::de::value::MapAccessDeserializer::new(visitor),
                )?;
                Ok(Drc::new(obj))
            }
        }

        deserializer.deserialize_any(StringOrStruct())
    }

    impl<'de> sd::Deserialize<'de> for DrcStored {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: sd::Deserializer<'de>,
        {
            Self::deserialize_with_dedup(deserializer, &DEDUP_STORED, |d| {
                string_or_struct_polymorphic(d)
            })
        }
    }

    // this test just uses the default inline mode of dedup
    #[test]
    fn serde_tagged_dedup_roundtrip_inline() {
        serde_tagged_dedup_roundtrip(
            DedupControl::Inline,
            HashMap::new(),
            r###"{
  "C": {
    "id": "d",
    "left": {
      "C": {
        "id": "c",
        "left": {
          "A": {
            "foo": "a"
          }
        },
        "right": {
          "B": {
            "Str": "b"
          }
        }
      }
    },
    "right": {
      "C": {
        "id": "c",
        "left": {
          "A": {
            "foo": "a"
          }
        },
        "right": {
          "B": {
            "Str": "b"
          }
        }
      }
    }
  }
}"###,
        )
    }

    #[test]
    fn serde_tagged_dedup_roundtrip_error_if_missing() {
        let a: Arc<Stored> = Arc::new(A {
            foo: "a".to_owned(),
        });
        let rc_a = DrcStored::new(RcStored::new(a.clone()));
        let b: Arc<Stored> = Arc::new(B::Str("b".to_owned()));
        let rc_b = DrcStored::new(RcStored::new(b.clone()));
        let c: Arc<Stored> = Arc::new(C {
            id: "c".to_string(),
            left: rc_a.clone(),
            right: rc_b.clone(),
        });
        let rc_c = DrcStored::new(RcStored::new(c.clone()));
        let d: Arc<Stored> = Arc::new(C {
            id: "d".to_string(),
            left: rc_c.clone(),
            right: rc_c.clone(),
        });
        let rc_d = DrcStored::new(RcStored::new(d.clone()));

        let mut map = HashMap::new();
        map.insert("a".to_string(), rc_a);
        map.insert("b".to_string(), rc_b);
        map.insert("c".to_string(), rc_c);
        map.insert("d".to_string(), rc_d);

        serde_tagged_dedup_roundtrip(DedupControl::ErrorIfMissing, map, r###""d""###);
    }

    #[test]
    fn serde_tagged_dedup_roundtrip_write_once() {
        serde_tagged_dedup_roundtrip(
            DedupControl::WriteOnce,
            HashMap::new(),
            r###"{
  "C": {
    "id": "d",
    "left": {
      "C": {
        "id": "c",
        "left": {
          "A": {
            "foo": "a"
          }
        },
        "right": {
          "B": {
            "Str": "b"
          }
        }
      }
    },
    "right": "c"
  }
}"###,
        );
    }

    fn serde_tagged_dedup_roundtrip(
        control: DedupControl,
        map: HashMap<String, DrcStored>,
        expected: &str,
    ) {
        // Let's begin by creating our test data ...
        let a: Arc<Stored> = Arc::new(A {
            foo: "a".to_owned(),
        });
        let rc_a = DrcStored::new(RcStored::new(a.clone()));
        let b: Arc<Stored> = Arc::new(B::Str("b".to_owned()));
        let rc_b = DrcStored::new(RcStored::new(b.clone()));
        let c: Arc<Stored> = Arc::new(C {
            id: "c".to_string(),
            left: rc_a.clone(),
            right: rc_b.clone(),
        });
        let rc_c = DrcStored::new(RcStored::new(c.clone()));
        let d: Arc<Stored> = Arc::new(C {
            id: "d".to_string(),
            left: rc_c.clone(),
            right: rc_c.clone(),
        });
        let rc_d = DrcStored::new(RcStored::new(d.clone()));

        // Now we can serialize our trait-objects.
        // Thanks to our `Serialize` implementation for trait objects this works
        // just like with any other type.
        let ser_a = to_string_pretty(&rc_a, control, &map);
        let ser_b = to_string_pretty(&rc_b, control, &map);
        let ser_c = to_string_pretty(&rc_c, control, &map);
        let ser_d = to_string_pretty(&rc_d, control, &map);

        let ser_d_str = String::from_utf8(ser_d.clone()).unwrap();
        print!("{}", ser_d_str);
        assert_eq!(ser_d_str, expected);

        // Now we let's deserialize our trait objects.
        // This works also just like any other type.
        let de_a: DrcStored = from_json(&ser_a, control, &map);
        let de_b: DrcStored = from_json(&ser_b, control, &map);
        let de_c: DrcStored = from_json(&ser_c, control, &map);
        let de_d: DrcStored = from_json(&ser_d, control, &map);

        assert_debug_eq(&rc_a, &de_a);
        assert_debug_eq(&rc_b, &de_b);
        assert_debug_eq(&rc_c, &de_c);
        assert_debug_eq(&rc_d, &de_d);
    }

    fn to_string_pretty(
        obj: &DrcStored,
        control: DedupControl,
        map: &HashMap<String, DrcStored>,
    ) -> Vec<u8> {
        let mut buffer = Vec::new();
        {
            let mut serializer = serde_json::Serializer::pretty(&mut buffer);
            let mut seed = Dedup::<Stored, Qrc<Stored>>::new(control.clone(), map.clone());
            seed.with(&DEDUP_STORED, || obj.serialize(&mut serializer))
                .unwrap();
        }
        buffer
    }

    fn from_json(ser: &[u8], control: DedupControl, map: &HashMap<String, DrcStored>) -> DrcStored {
        let mut deserializer = serde_json::Deserializer::from_slice(&ser);
        let mut seed = Dedup::<Stored, Qrc<Stored>>::new(control.clone(), map.clone());
        seed.with(&DEDUP_STORED, || DrcStored::deserialize(&mut deserializer))
            .unwrap()
    }

    /// A helper function to assert that two strings contain the same JSON data.
    fn assert_json_equal(a: &str, b: &str) {
        let a: serde_json::Value = serde_json::from_str(a).unwrap();
        let b: serde_json::Value = serde_json::from_str(b).unwrap();
        assert_eq!(a, b);
    }

    /// A helper function to assert that the debug representations of two objects
    /// are the same
    pub fn assert_debug_eq<T>(a: &T, b: &T)
    where
        T: Debug,
    {
        let a_debug = format!("{:?}", a);
        let b_debug = format!("{:?}", b);
        assert_eq!(a_debug, b_debug);
    }
}
