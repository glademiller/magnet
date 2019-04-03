# OpenAPI JSON schema generator

This is a fork of magnet in order to provide a JSON Schema generator suitable for OpenAPI v3 specifications

The defined `JsonSchema` trait defines a single function, `json_schema`, which should/will return a `serde_json::Value` that is a valid JSON schema describing the structure of the implementing type. Example:

```rust
#[macro_use]
extern crate serde_derive;
extern crate serde;
#[macro_use]
extern crate magnet_derive;
extern crate openapi_json_schema;
extern crate mongodb;

use std::collections::HashSet;
use openapi_json_schema::JsonSchema;

use mongodb::{ Client, ThreadedClient, CommandType };
use mongodb::db::{ ThreadedDatabase };

#[derive(JsonSchema)]
struct Person {
    name: String,
    nicknames: HashSet<String>,
    age: usize,
    contact: Option<Contact>,
}

#[derive(JsonSchema, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
enum Contact {
    Email(String),
    Phone(u64),
}

fn main() {
    let schema = Person::json_schema();
    let spec = json! ({
        "create": "Person",
        "validator": { "$jsonSchema": schema },
    });
    let client = Client::connect("localhost", 27017).expect("can't connect to mongod");
    let db = client.db("Example");
    db.command(spec, CommandType::CreateCollection, None).expect("network error");
    // etc.
}
```

For milestones and custom `#[attributes]`, please see the [documentation](https://docs.rs/openapi_json_schema).

## Release Notes

### v0.8.0

* Implement `JsonSchema` for `VecDeque`, `BinaryHeap`, `LinkedList`, `Range`, `RangeInclusive`, and `PhantomData`
* Add `Eq + Hash` and `Ord` bounds on map keys and set elements where appropriate

### v0.7.0

* Upgrade `uuid` dependency to 0.7.1, and include `v4` and `serde` features
* Upgrade `url` dependency to `1.7.2`

### v0.6.0

* `impl JsonSchema` for arrays of size 2<sup>N</sup> between 128 and 65536; and sizes 1.5 * 2<sup>N</sup> between 96 and 1536.
* Rewrite generics handling using `syn::Generics::split_for_impl`
* Use scoped lints in `openapi_json_schema` as well

### v0.5.0

* Handle generic types with default generic parameters correctly, by not including the defaults in the generated `impl` (which would result in a compiler error)
* Use scoped lints for Clippy
* Update some dependencies

### v0.4.0

* Update `bson` to `0.13.0` and require its `u2i` feature. This version fixes a
  bug where unit struct were serialized as 1-element arrays. The `u2i` feature
  allows unsigned integers (within the appropriate range) to be serialized as
  signed integers.

### v0.3.3

* Fix a bug where `Option<enum>` was not allowed to be `null`/`None` by the
  generated BSON schema
* Remove an incorrect item from the documentation
* Fix several Clippy lints
* Update dependencies

### v0.3.2

* `impl JsonSchema for Document`
* `impl JsonSchema for ObjectId`
* Documentation improvements
* Update dependencies

### v0.3.1

* Relax `Display` bound for `HashMap`/`BTreeMap` keys, use `ToString` instead
* Update `proc_macro2` dependency so that we can use `TokenStream::default()`

### v0.3.0

* Remove `#[magnet(rename = "...")]` attribute
* `UnorderedDoc::eq()`, `assert_doc_eq!` and `assert_doc_ne!` no longer clone their arguments
* Update `syn` and `quote` dependencies
* Improve documentation

### v0.2.1

* Update `bson` dependency

### v0.2.0

* Support for generic types

### v0.1.4

* Unit tests and a test suite have been added.
* Bug fix: `Option::json_schema()` didn't handle the `bsonType` field, so `Option<integer>` wasn't allowed to be `null`. This has been corrected.
* Bug fix: every generated schema now uses `Bson::I64` for representing array lengths / collection counts
* Enhancement: `impl JsonSchema for { HashMap, BTreeMap }` now has a less stringent trait bound on the key. It is now `Display` instead of `AsRef<str>`.

### v0.1.3

* Add support for `#[magnet(min_incl = "...", min_excl = "...", max_incl = "...", max_excl = "...")]` attributes on struct fields (named as well as newtype and tuple)

### v0.1.2

* Add support for `enum`s, respecting Serde's tagging conventions (untagged/external/internal/adjacent), except newtype variants around other (inner) `enum`s
* Refactoring, code quality improvements

### v0.1.1

* Add support for newtype structs and tuple structs
* Respect `#[serde(rename_all = "...")]` and `#[serde(rename = "...")]` attributes
* Add Serde-conform case conversion
* Code formatting / organization and documentation improvements

### v0.1.0

* Initial release, only regular structs with named fields are supported
