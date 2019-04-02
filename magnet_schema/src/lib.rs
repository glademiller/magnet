//! # Magnet, a JSON/BSON schema generator
//!
//! These two related crates, `magnet_schema` and `magnet_derive` define
//! a trait, `JsonSchema`, and a proc-macro derive for the same trait,
//! which allows types to easily implement JSON schema validation for
//! use with MongoDB.
//!
//! The trait defines a single function, `json_schema()`, that returns
//! a BSON `Document` describing the validation schema of the type based
//! on its fields (for `struct`s and tuples), variants (for `enum`s), or
//! elements/entries (for array- and map-like types).
//!
//! The types are expected to be serialized and deserialized using Serde,
//! and generally Magnet will try very hard to respect `#[serde(...)]`
//! annotations as faithfully as possible, but no `Serialize + Deserialize`
//! trait bounds are enforced on the types as this is not strictly necessary.
//!
//! ## Usage Example
//!
//! ```rust
//! #[macro_use]
//! extern crate serde_derive;
//! extern crate serde;
//! #[macro_use]
//! extern crate serde_json;
//! #[macro_use]
//! extern crate magnet_derive;
//! extern crate magnet_schema;
//!
//! use std::collections::HashSet;
//! use magnet_schema::JsonSchema;
//!
//! #[derive(JsonSchema)]
//! struct Person {
//!     name: String,
//!     nicknames: HashSet<String>,
//!     age: usize,
//!     contact: Option<Contact>,
//! }
//!
//! #[derive(JsonSchema, Serialize, Deserialize)]
//! #[serde(tag = "type", content = "value")]
//! enum Contact {
//!     Email(String),
//!     Phone(u64),
//! }
//!
//! fn main() {
//!     println!("{:#?}", Person::json_schema());
//! }
//! ```
//!
//! ## Custom Attributes
//!
//! * `#[serde(rename = "new_name")]`: Magnet will respect Serde's field/variant
//!   renaming attribute by default.
//!
//! * `#[serde(rename_all = "rename_rule")]`: it will also respect Serde's
//!   `rename_all` rule.
//!
//! * `#[magnet(min_incl = "-1337")]` &mdash; enforces an inclusive minimum for fields of numeric types
//!
//! * `#[magnet(min_excl = "42")]` &mdash; enforces an exclusive "minimum" (infimum) for fields of numeric types
//!
//! * `#[magnet(max_incl = "63")]` &mdash; enforces an inclusive maximum for fields of numeric types
//!
//! * `#[magnet(max_excl = "64")]` &mdash; enforces an exclusive "maximum" (supremum) for fields of numeric types
//!
//! ## Development Roadmap
//!
//! * `[x]` Define `JsonSchema` trait
//!
//! * `[x]` `impl JsonSchema` for most primitives/`std::` types
//!
//! * `[x]` Cargo `feature`s for implementing `JsonSchema` for "atomic"
//!   types in foreign crates, for instance, `url::Url` and `uuid::Uuid`.
//!
//! * `[x]` `#[derive(JsonSchema)]` on regular, named-field structs
//!
//! * `[x]` `#[derive(JsonSchema)]` on newtype structs
//!
//! * `[x]` `#[derive(JsonSchema)]` on tuple structs
//!
//! * `[x]` `#[derive(JsonSchema)]` on unit structs
//!
//! * `[ ]` `#[derive(JsonSchema)]` on enums
//!
//!   * `[x]` unit variants
//!
//!   * `[ ]` newtype variants
//!
//!     * `[x]` newtype variants around structs and maps
//!
//!     * `[ ]` newtype variants around inner, transitive `enum`s
//!
//!   * `[x]` tuple variants
//!
//!   * `[x]` struct variants
//!
//!   * `[x]` respect Serde tagging conventions: external/internal/adjacent
//!
//! * `[x]` Respect more `#[serde(...)]` attributes, for example: `rename`,
//!   `rename_all`
//!
//! * `[ ]` Respect more `#[serde(...)]` attributes, for example: `default`,
//!   `skip`, `skip_serializing`, `skip_deserializing`
//!
//! * `[x]` Handle generic types in proc-macro derive
//!
//! * `[ ]` Standard (non-MongoDB-specific) JSON schema support (approach?)
//!
//! * `[x]` unit tests
//!
//! * `[x]` documentation for attributes
//!
//! * `[ ]` `impl JsonSchema` for more esoteric primitives/standard types
//!   such as specialization of `[u8]`/`Vec<u8>` as binary, adding a
//!   validation regex `"pattern"` to `Path` and `PathBuf`, etc.
//!
//! * `[ ]` Add our own attributes
//!
//!   * `[x]` `magnet(rename = "...")` &mdash; renames the field or variant
//!     to the name specified as the value of the `rename` attribute
//!
//!   * `[ ]` `magnet(regex = "foo?|[ba]r{3,6}")` &mdash; custom validation;
//!     implies `"type": "string"`. Patterns are implicitly enclosed between
//!     `^...$` for robustness.
//!
//!   * `[ ]` `magnet(unsafe_regex = "^nasty-regex$")` &mdash; just like
//!     `magnet(regex)`, but no automatic enclosing in `^...$` happens.
//!     **This may allow invalid data to pass validation!!!**
//!
//!   * `[ ]` `magnet(non_empty)` &mdash; for collections: same as `min_length = "1"`.
//!
//!   * `[ ]` `magnet(min_length = "16")` &mdash; for collections/tuples etc.
//!
//!   * `[ ]` `magnet(max_length = "32")` &mdash; for collections/tuples etc.
//!
//!   * `[x]` `magnet(min_incl = "-1337")` &mdash; inclusive minimum for numbers
//!
//!   * `[x]` `magnet(min_excl = "42")` &mdash; exclusive "minimum" (infimum) for numbers
//!
//!   * `[x]` `magnet(max_incl = "63")` &mdash; inclusive maximum for numbers
//!
//!   * `[x]` `magnet(max_excl = "64")` &mdash; exclusive "maximum" (supremum) for numbers
//!
//!   * `[ ]` `magnet(allow_extra_fields)` &mdash; sets `"additionalProperties": true`.
//!     By default, Magnet sets this field to `false` for maximal safety.
//!     Allowing arbitrary data to be inserted in a DB is generally a Bad Idea,
//!     as it may lead to code injection (`MongoDB` supports storing JavaScript
//!     in a collection! Madness!) or at best, denial-of-service (DoS) attacks.
//!
//!   * `[ ]` `magnet(allow_extra_fields = "ExtraFieldType")` &mdash; sets
//!     `"additionalProperties": ExtraFieldType::json_schema()`, so that
//!     unlisted additional object fields are allowed provided that they
//!     conform to the schema of the specified type.

#![doc(html_root_url = "https://docs.rs/magnet_schema/0.8.0")]
#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unstable_features,
    unused_import_braces,
    unused_qualifications,
    missing_docs
)]
#![allow(
    clippy::single_match,
    clippy::match_same_arms,
    clippy::match_ref_pats,
    clippy::clone_on_ref_ptr,
    clippy::needless_pass_by_value
)]
#![deny(
    clippy::wrong_pub_self_convention,
    clippy::used_underscore_binding,
    clippy::stutter,
    clippy::similar_names,
    clippy::pub_enum_variant_names,
    clippy::missing_docs_in_private_items,
    clippy::non_ascii_literal,
    clippy::unicode_not_nfc,
    clippy::result_unwrap_used,
    clippy::option_unwrap_used,
    clippy::option_map_unwrap_or_else,
    clippy::option_map_unwrap_or,
    clippy::filter_map,
    clippy::shadow_unrelated,
    clippy::shadow_reuse,
    clippy::shadow_same,
    clippy::int_plus_one,
    clippy::string_add_assign,
    clippy::if_not_else,
    clippy::invalid_upcast_comparisons,
    clippy::cast_precision_loss,
    clippy::cast_lossless,
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::mutex_integer,
    clippy::mut_mut,
    clippy::items_after_statements,
    clippy::print_stdout,
    clippy::mem_forget,
    clippy::maybe_infinite_iter
)]

#[cfg(feature = "url")]
extern crate url;
#[cfg(feature = "uuid")]
extern crate uuid;
#[macro_use]
extern crate serde_json;

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque};
use std::ffi::{OsStr, OsString};
use std::hash::{BuildHasher, Hash};
use std::marker::PhantomData;
use std::ops::{Range, RangeInclusive};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};
use std::{i16, i32, i64, i8, isize, u16, u32, u64, u8, usize};

#[doc(hidden)]
pub mod support;

/// Types which can be expressed/validated by a MongoDB-flavored JSON schema.
pub trait JsonSchema {
    /// Returns a BSON document describing the MongoDB-flavored schema of this type.
    fn json_schema() -> serde_json::Value;
}

/////////////////////////////
// Primitive and std types //
/////////////////////////////

impl JsonSchema for bool {
    fn json_schema() -> serde_json::Value {
        json! ({ "type": "boolean" })
    }
}

macro_rules! impl_json_schema_int {
    ($($ty:ident: $format:expr, $min:expr => $max:expr;)*) => {$(
        impl JsonSchema for $ty {
            #[allow(trivial_numeric_casts)]
            #[allow(clippy::cast_possible_wrap, clippy::cast_lossless)]
            fn json_schema() -> serde_json::Value {
                json! ({
                    "format": $format,
                    "minimum": $min as i64,
                    "maximum": $max as i64,
                })
            }
        }
    )*}
}

impl_json_schema_int! {
    u8 : "uint8" , u8::MIN  =>  u8::MAX;
    u16: "uint16" , u16::MIN => u16::MAX;
    u32: "uint32" , u32::MIN => u32::MAX;
    u64: "uint64" , u64::MIN => i64::MAX; // !!! must not overflow i64
    i8 : "int8"   , i8::MIN  =>  i8::MAX;
    i16: "int16"  , i16::MIN => i16::MAX;
    i32: "int32"  , i32::MIN => i32::MAX;
    i64: "int64"  , i64::MIN => i64::MAX;
}

#[cfg(any(
    target_pointer_width = "8",
    target_pointer_width = "16",
    target_pointer_width = "32"
))]
impl JsonSchema for usize {
    fn json_schema() -> serde_json::Value {
        json! ({
            "format": "int64",
            "minimum": usize::MIN as i64,
            "maximum": usize::MAX as i64,
        })
    }
}

/// Do **NOT** assume `sizeof(usize) <= sizeof(u64)`!!!
#[cfg(target_pointer_width = "64")]
impl JsonSchema for usize {
    fn json_schema() -> serde_json::Value {
        json! ({

            "format": "int64",
            "minimum": usize::MIN as i64,
            "maximum": isize::MAX as i64,
        })
    }
}

/// Do **NOT** assume `sizeof(isize) <= sizeof(i64)`!!!
#[cfg(any(
    target_pointer_width = "8",
    target_pointer_width = "16",
    target_pointer_width = "32",
    target_pointer_width = "64"
))]
impl JsonSchema for isize {
    fn json_schema() -> serde_json::Value {
        json! ({
            "format": "int64",
            "minimum": isize::MIN as i64,
            "maximum": isize::MAX as i64,
        })
    }
}

macro_rules! impl_json_schema_float {
    ($($ty:ident : $format:expr , $min:expr => $max:expr;)*) => {$(
        impl JsonSchema for $ty {
            fn json_schema() -> serde_json::Value {
                json!({ "type": "number", "format": $format, "minimum": $min, "maximum": $max })
            }
        }
    )*}
}

impl_json_schema_float! {
    f32: "float", std::f32::MIN => std::f32::MAX;
    f64: "double", std::f64::MIN => std::f64::MAX;
}

macro_rules! impl_json_schema_string {
    ($($ty:ty,)*) => {$(
        impl JsonSchema for $ty {
            fn json_schema() -> serde_json::Value {
                json!({ "type": "string" })
            }
        }
    )*}
}

// TODO(H2CO3): path-matching regex for `Path` and `PathBuf`?
impl_json_schema_string! {
    str,
    String,
    OsStr,
    OsString,
    Path,
    PathBuf,
}

///////////////////////////////
// Built-in parametric types //
///////////////////////////////

impl<'a, T> JsonSchema for &'a T
where
    T: ?Sized + JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        T::json_schema()
    }
}

impl<'a, T> JsonSchema for &'a mut T
where
    T: ?Sized + JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        T::json_schema()
    }
}

/// TODO(H2CO3): maybe specialize as binary for `[u8]`?
impl<T> JsonSchema for [T]
where
    T: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "items": T::json_schema(),
        })
    }
}

macro_rules! impl_json_schema_array {
    ($($size:expr,)*) => {$(
        impl<T> JsonSchema for [T; $size] where T: JsonSchema {
            #[allow(trivial_numeric_casts)]
            fn json_schema() -> serde_json::Value {
                json! ({
                    "type": "array",
                    "minItems": $size as i64,
                    "maxItems": $size as i64,
                    "items": T::json_schema(),
                })
            }
        }
    )*}
}

impl_json_schema_array! {
    0,   1,  2,  3,  4,  5,  6,  7,
    8,   9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23,
    24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61, 62, 63,
}

impl_json_schema_array! {
    64,     96,  128,  192,  256,   384,   512,   768,
    1024, 1536, 2048, 4096, 8192, 16384, 32768, 65536,
}

impl JsonSchema for () {
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": ["array", "null"],
            "maxItems": 0_i64,
        })
    }
}

macro_rules! impl_json_schema_tuple {
    ($($ty:ident),*) => {
        impl<$($ty),*> JsonSchema for ($($ty),*) where $($ty: JsonSchema),* {
            fn json_schema() -> serde_json::Value {
                json! ({
                    "type": "array",
                    "additionalItems": false,
                    "items": [$($ty::json_schema()),*],
                })
            }
        }
    }
}

impl_json_schema_tuple! { A, B }
impl_json_schema_tuple! { A, B, C }
impl_json_schema_tuple! { A, B, C, D }
impl_json_schema_tuple! { A, B, C, D, E }
impl_json_schema_tuple! { A, B, C, D, E, F }
impl_json_schema_tuple! { A, B, C, D, E, F, G }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J, K }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J, K, L }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O }
impl_json_schema_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }

///////////////////////////////////////
// Generics, Containers, Collections //
///////////////////////////////////////

/// TODO(H2CO3): maybe specialize for `Cow<[u8]>` as binary?
impl<'a, T> JsonSchema for Cow<'a, T>
where
    T: ?Sized + Clone + JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        T::json_schema()
    }
}

impl<T> JsonSchema for Cell<T>
where
    T: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        T::json_schema()
    }
}

macro_rules! impl_json_schema_unsized {
    ($($ty:ident,)*) => {$(
        impl<T> JsonSchema for $ty<T> where T: ?Sized + JsonSchema {
            fn json_schema() -> serde_json::Value {
                T::json_schema()
            }
        }
    )*}
}

impl_json_schema_unsized! {
    Box,
    Rc,
    Arc,
    RefCell,
    Mutex,
    RwLock,
}

/// TODO(H2CO3): maybe specialize for `Vec<u8>` as binary?
impl<T> JsonSchema for Vec<T>
where
    T: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "items": T::json_schema(),
        })
    }
}

impl<T> JsonSchema for VecDeque<T>
where
    T: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "items": T::json_schema(),
        })
    }
}

impl<T> JsonSchema for LinkedList<T>
where
    T: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "items": T::json_schema(),
        })
    }
}

impl<T> JsonSchema for BinaryHeap<T>
where
    T: JsonSchema + Ord,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "items": T::json_schema(),
        })
    }
}

impl<T> JsonSchema for Option<T>
where
    T: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        let mut doc = T::json_schema();
        let null_str = serde_json::Value::String("null".to_string());
        let mut obj = doc.as_object().expect("must be an object").clone();
        let (type_key, old_type_spec) = match obj.remove("type") {
            Some(spec) => ("type", spec),
            None => {
                // type wasn't directly constrained;
                // as a last resort, check if it's an `enum`.
                if let Some(&mut serde_json::Value::Array(ref mut array)) = doc.get_mut("anyOf") {
                    array.push(json!({ "type": null_str }));
                }
                return doc;
            }
        };
        let new_type_spec = match old_type_spec {
            serde_json::Value::String(_) => vec![old_type_spec, null_str],
            serde_json::Value::Array(mut array) => {
                // duplicate type strings are a schema error :(
                if !array.iter().any(|item| item == &null_str) {
                    array.push(null_str);
                }

                array
            }
            _ => panic!(
                "invalid schema: `{}` isn't a string or array: {:?}",
                type_key, old_type_spec
            ),
        };

        obj.insert(
            type_key.to_string(),
            serde_json::Value::Array(new_type_spec),
        );
        serde_json::Value::Object(obj.clone())
    }
}

impl<T, H> JsonSchema for HashSet<T, H>
where
    T: JsonSchema + Eq + Hash,
    H: BuildHasher,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "uniqueItems": true,
            "items": T::json_schema(),
        })
    }
}

impl<T> JsonSchema for BTreeSet<T>
where
    T: JsonSchema + Ord,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "array",
            "uniqueItems": true,
            "items": T::json_schema(),
        })
    }
}

impl<K, V, H> JsonSchema for HashMap<K, V, H>
where
    K: ToString + Eq + Hash,
    V: JsonSchema,
    H: BuildHasher,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "object",
            "additionalProperties": V::json_schema(),
        })
    }
}

impl<K, V> JsonSchema for BTreeMap<K, V>
where
    K: ToString + Ord,
    V: JsonSchema,
{
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "object",
            "additionalProperties": V::json_schema(),
        })
    }
}

impl<T: JsonSchema> JsonSchema for Range<T> {
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": ["start", "end"],
            "properties": {
                "start": T::json_schema(),
                "end":   T::json_schema(),
            },
        })
    }
}

impl<T: JsonSchema> JsonSchema for RangeInclusive<T> {
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": ["start", "end"],
            "properties": {
                "start": T::json_schema(),
                "end":   T::json_schema(),
            },
        })
    }
}

impl<T> JsonSchema for PhantomData<T> {
    fn json_schema() -> serde_json::Value {
        // it's just a unit struct
        <() as JsonSchema>::json_schema()
    }
}

////////////////////////////////////////////////////////
// Implementations for useful types in foreign crates //
////////////////////////////////////////////////////////

#[cfg(feature = "url")]
impl JsonSchema for url::Url {
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "string",
            "format": "uri",
            // TODO(H2CO3): validation regex pattern?
        })
    }
}

#[cfg(feature = "uuid")]
impl JsonSchema for uuid::Uuid {
    fn json_schema() -> serde_json::Value {
        json! ({
            "type": "string",
            "format": "uuid4"
            "pattern": "^[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}$",
        })
    }
}
