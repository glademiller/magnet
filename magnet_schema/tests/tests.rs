#![recursion_limit = "128"]
#![allow(clippy::cast_lossless)]

extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate magnet_derive;
extern crate magnet_schema;
#[macro_use]
extern crate serde_json;

use magnet_schema::JsonSchema;
use std::str;

#[test]
fn unordered_doc_equality() {
    let d1 = json! ({
        "foo": "bar",
        "qux": 42,
        "key": [
            {
                "inner_1": null,
                "inner_2": 1337,
            },
            {
                "inner_3": "value",
                "inner_4": -42,
            },
        ],
        "inner": {
            "one": false,
            "other": true,
        },
    });

    let d2 = json! ({
        "key": [
            {
                "inner_2": 1337,
                "inner_1": null,
            },
            {
                "inner_4": -42,
                "inner_3": "value",
            },
        ],
        "foo": "bar",
        "qux": 42,
        "inner": {
            "other": true,
            "one": false,
        },
    });

    let d3 = json! ({
        "key": [
            {
                "inner_3": "value",
                "inner_4": -42,
            },
            {
                "inner_1": null,
                "inner_2": 1337,
            },
        ],
        "foo": "bar",
        "qux": 42,
        "inner": {
            "other": true,
            "one": false,
        },
    });
    assert_eq!(d1, d2);
    assert_eq!(d2, d1);

    assert_ne!(d1, d3);
    assert_ne!(d3, d1);

    assert_ne!(d2, d3);
    assert_ne!(d3, d2);
}

#[test]
fn unit_struct() {
    use std::marker::PhantomData;

    #[derive(JsonSchema)]
    struct FstUnit;

    #[derive(JsonSchema)]
    struct SndUnit();

    /// intentionally no impl or derive `JsonSchema` - it shouldn't be required!
    struct PhantomInner;

    let unit_schema = json! ({
        "type": ["array", "null"],
        "maxItems": 0_i64,
    });

    let fst_schema = FstUnit::json_schema();
    let snd_schema = SndUnit::json_schema();
    let phantom_schema = PhantomData::<PhantomInner>::json_schema();

    assert_eq!(fst_schema, snd_schema);
    assert_eq!(snd_schema, fst_schema);

    assert_eq!(fst_schema, unit_schema);
    assert_eq!(snd_schema, unit_schema);

    assert_eq!(phantom_schema, unit_schema);
    assert_eq!(unit_schema, phantom_schema);
}

#[test]
fn newtype_struct() {
    #[derive(JsonSchema)]
    struct FloatingPoint(f64);

    #[derive(JsonSchema)]
    struct Angle(#[magnet(min_incl = "-180", max_excl = "180")] f32);

    assert_eq!(FloatingPoint::json_schema(), f64::json_schema());

    assert_eq!(
        Angle::json_schema(),
        json! ({
            "type": "number",
            "format": "float",
            "minimum": -180.0,
            "exclusiveMinimum": false,
            "maximum": 180.0,
            "exclusiveMaximum": true,
        })
    );
}

#[test]
fn tuple_struct() {
    #[derive(JsonSchema)]
    struct Complex(f64, f64);

    #[derive(JsonSchema)]
    struct IntRange(Option<u32>, Option<u32>);

    assert_eq!(
        Complex::json_schema(),
        json! ({
            "type": "array",
            "additionalItems": false,
            "items": [
                { "type": "number", "format": "double", "maximum": std::f64::MAX, "minimum": std::f64::MIN },
                { "type": "number", "format": "double", "maximum": std::f64::MAX, "minimum": std::f64::MIN },
            ],
        })
    );

    assert_eq!(
        IntRange::json_schema(),
        json! ({
            "type": "array",
            "additionalItems": false,
            "items": [
                {
                    "minimum": std::u32::MIN as i64,
                    "maximum": std::u32::MAX as i64,
                    "format": "uint32"
                },
                {
                    "minimum": std::u32::MIN as i64,
                    "maximum": std::u32::MAX as i64,
                    "format": "uint32"
                },
            ],
        })
    );
}

#[test]
fn struct_with_named_fields() {
    use std::collections::BTreeMap;

    #[derive(JsonSchema)]
    #[allow(dead_code)]
    struct Contact {
        names: Vec<String>,
        address_lines: [String; 3],
        phone_no: Option<u64>,
        email: Option<Email>,
        misc_info: Option<BTreeMap<String, String>>,
    }

    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(rename_all = "SCREAMING-KEBAB-CASE")]
    struct Email {
        #[serde(rename = "aDdReSs")]
        address: String,
        provider_name: String,
    }

    assert_eq!(
        Contact::json_schema(),
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": [
                "names",
                "address_lines",
                "phone_no",
                "email",
                "misc_info",
            ],
            "properties": {
                "names": {
                    "type": "array",
                    "items": {
                        "type": "string",
                    }
                },
                "address_lines": {
                    "type": "array",
                    "items": {
                        "type": "string",
                    },
                    "minItems": 3 as i64,
                    "maxItems": 3 as i64,
                },
                "phone_no": {
                    "format": "uint64",
                    "minimum": std::u64::MIN as i64,
                    "maximum": std::i64::MAX,
                },
                "email": {
                    "type": ["object", "null"],
                    "additionalProperties": false,
                    "required": ["aDdReSs", "PROVIDER-NAME"],
                    "properties": {
                        "aDdReSs": { "type": "string" },
                        "PROVIDER-NAME": { "type": "string" },
                    },
                },
                "misc_info": {
                    "type": ["object", "null"],
                    "additionalProperties": {
                        "type": "string",
                    },
                },
            },
        })
    );
}

#[test]
fn untagged_enum() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(untagged)]
    enum Untagged {
        Unit,
        NewType(Option<String>),
        TwoTuple(u8, i16),
        Struct { field: i32 },
    }

    assert_eq!(
        Untagged::json_schema(),
        json! ({
            "anyOf": [
                {
                    "type": ["array", "null"],
                    "maxItems": 0_i64,
                },
                {
                    "type": ["string", "null"],
                },
                {
                    "type": "array",
                    "additionalItems": false,
                    "items": [
                        {
                            "format": "uint8",
                            "minimum": std::u8::MIN as i64,
                            "maximum": std::u8::MAX as i64,
                        },
                        {
                            "format": "int16",
                            "minimum": std::i16::MIN as i64,
                            "maximum": std::i16::MAX as i64,
                        },
                    ],
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": [ "field" ],
                    "properties": {
                        "field": {
                            "format": "int32",
                            "minimum": std::i32::MIN as i64,
                            "maximum": std::i32::MAX as i64,
                        },
                    },
                },
            ]
        })
    );
}

#[test]
fn externally_tagged_enum() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(rename_all = "snake_case")]
    enum ExternallyTagged {
        Unit,
        NewType(Option<String>),
        TwoTuple(u8, i16),
        Struct { field: i32 },
    }

    assert_eq!(
        ExternallyTagged::json_schema(),
        json! ({
            "anyOf": [
                {
                    "enum": ["unit"],
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": [ "new_type" ],
                    "properties": {
                        "new_type": {
                            "type": ["string", "null"],
                        },
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["two_tuple"],
                    "properties": {
                        "two_tuple": {
                            "type": "array",
                            "additionalItems": false,
                            "items": [
                                {
                                    "format": "uint8",
                                    "minimum": std::u8::MIN as i64,
                                    "maximum": std::u8::MAX as i64,
                                },
                                {
                                    "format": "int16",
                                    "minimum": std::i16::MIN as i64,
                                    "maximum": std::i16::MAX as i64,
                                },
                            ],
                        },
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["struct"],
                    "properties": {
                        "struct": {
                            "type": "object",
                            "additionalProperties": false,
                            "required": ["field"],
                            "properties": {
                                "field": {
                                    "format": "int32",
                                    "minimum": std::i32::MIN as i64,
                                    "maximum": std::i32::MAX as i64,
                                },
                            },
                        },
                    },
                },
            ]
        })
    );
}

#[test]
fn adjacently_tagged_enum() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(rename_all = "snake_case", tag = "variant", content = "value")]
    enum AdjacentlyTagged {
        Unit,
        NewType(Option<String>),
        TwoTuple(u8, i16),
        Struct { field: i32 },
    }

    assert_eq!(
        AdjacentlyTagged::json_schema(),
        json! ({
            "anyOf": [
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant"],
                    "properties": {
                        "variant": { "enum": ["unit"] },
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant", "value"],
                    "properties": {
                        "variant": { "enum": ["new_type"] },
                        "value": { "type": ["string", "null"] },
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant", "value"],
                    "properties": {
                        "variant": { "enum": ["two_tuple"] },
                        "value": {
                            "type": "array",
                            "additionalItems": false,
                            "items": [
                                {
                                    "format": "uint8",
                                    "minimum": std::u8::MIN as i64,
                                    "maximum": std::u8::MAX as i64,
                                },
                                {
                                    "format": "int16",
                                    "minimum": std::i16::MIN as i64,
                                    "maximum": std::i16::MAX as i64,
                                },
                            ],
                        },
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant", "value"],
                    "properties": {
                        "variant": { "enum": ["struct"] },
                        "value": {
                            "type": "object",
                            "additionalProperties": false,
                            "required": ["field"],
                            "properties": {
                                "field": {
                                    "format": "int32",
                                    "minimum": std::i32::MIN as i64,
                                    "maximum": std::i32::MAX as i64,
                                },
                            },
                        },
                    },
                },
            ]
        })
    );
}

#[test]
fn internally_tagged_enum() {
    use std::collections::HashMap;

    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(rename_all = "snake_case", tag = "variant")]
    enum InternallyTagged {
        Unit,
        NewTypeOne(NewType),
        NewTypeTwo(HashMap<String, bool>),
        Struct { field: i32 },
    }

    #[derive(Serialize, Deserialize, JsonSchema)]
    struct NewType {
        name: String,
    }

    assert_eq!(
        InternallyTagged::json_schema(),
        json! ({
            "anyOf": [
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant"],
                    "properties": {
                        "variant": { "enum": ["unit"] },
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["name", "variant"],
                    "properties": {
                        "variant": { "enum": [ "new_type_one" ] },
                        "name": { "type": "string" },
                    },
                },
                {
                    "type": "object",
                    "required": ["variant"],
                    "properties": {
                        "variant": { "enum": [ "new_type_two" ] },
                    },
                    "additionalProperties": {
                        "type": "boolean",
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant", "field"],
                    "properties": {
                        "variant": { "enum": [ "struct" ] },
                        "field": {
                            "format": "int32",
                            "minimum": std::i32::MIN as i64,
                            "maximum": std::i32::MAX as i64,
                        },
                    },
                },
            ]
        })
    );
}

#[test]
#[should_panic]
fn malformed_internally_tagged_enum_1() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(tag = "variant")]
    enum Foo {
        Bar(Lol),
    }

    #[derive(Serialize, Deserialize, JsonSchema)]
    struct Lol;

    Foo::json_schema();
}

#[test]
#[should_panic]
fn malformed_internally_tagged_enum_2() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(tag = "variant")]
    enum Foo {
        Bar(u32),
    }

    Foo::json_schema();
}

#[test]
#[should_panic]
fn malformed_internally_tagged_enum_3() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(tag = "variant")]
    enum Foo {
        Bar(Option<S>),
    }

    #[derive(Serialize, Deserialize, JsonSchema)]
    struct S {
        f: bool,
    }

    Foo::json_schema();
}

#[test]
#[should_panic]
fn malformed_internally_tagged_enum_4() {
    #[derive(Serialize, Deserialize, JsonSchema)]
    #[serde(tag = "variant")]
    enum Foo {
        Bar(E),
    }

    #[derive(Serialize, Deserialize, JsonSchema)]
    enum E {
        Qux,
        Moo,
    }

    Foo::json_schema();
}

#[test]
fn generic_struct() {
    #[allow(dead_code)]
    #[derive(JsonSchema)]
    struct Generic<'a, 'b: 'a, T: 'a, U = u32> {
        ts: &'a [T],
        title: &'b str,
        other: U,
    }

    assert_eq!(
        Generic::<Option<f32>, Box<u16>>::json_schema(),
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": [
                "ts",
                "title",
                "other",
            ],
            "properties": {
                "ts": {
                    "type": "array",
                    "items": {
                        "type": ["number", "null"],
                        "format": "float",
                        "maximum": std::f32::MAX,
                        "minimum": std::f32::MIN
                    },
                },
                "title": { "type": "string" },
                "other": {
                    "format": "uint16",
                    "minimum": std::u16::MIN as i64,
                    "maximum": std::u16::MAX as i64,
                },
            },
        })
    );

    assert_eq!(
        Generic::<f64>::json_schema(),
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": [
                "ts",
                "title",
                "other",
            ],
            "properties": {
                "ts": {
                    "type": "array",
                    "items": {
                        "type": "number",
                        "format": "double",
                        "maximum": std::f64::MAX,
                        "minimum": std::f64::MIN
                    },
                },
                "title": { "type": "string" },
                "other": {
                    "format": "uint32",
                    "minimum": std::u32::MIN as i64,
                    "maximum": std::u32::MAX as i64,
                },
            },
        })
    );
}

#[test]
fn generic_enum() {
    use std::collections::{BTreeMap, HashMap};

    #[allow(dead_code)]
    #[derive(JsonSchema, Serialize)]
    #[serde(tag = "kind")]
    enum EitherRefMut<'a, 'b: 'a, L, R = BTreeMap<&'a str, bool>>
    where
        L: 'a,
        R: 'b,
    {
        Left(&'a mut L),
        Right(&'b mut R),
    }

    type E<'life1, 'life2> = EitherRefMut<'life1, 'life2, HashMap<String, ()>>;

    assert_eq!(
        E::json_schema(),
        json! ({
            "anyOf": [
                {
                    "type": "object",
                    "additionalProperties": {
                        "type": ["array", "null"],
                        "maxItems": 0_i64,
                    },
                    "required": ["kind"],
                    "properties": {
                        "kind": {
                            "enum": ["Left"],
                        }
                    },
                },
                {
                    "type": "object",
                    "additionalProperties": {
                        "type": "boolean",
                    },
                    "required": ["kind"],
                    "properties": {
                        "kind": {
                            "enum": ["Right"],
                        }
                    },
                },
            ]
        })
    );
}

#[test]
fn serde_rename_struct_field() {
    #[derive(Serialize, JsonSchema)]
    struct Foo {
        #[serde(rename = "newname")]
        field: i32,
    }

    assert_eq!(
        Foo::json_schema(),
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": ["newname"],
            "properties": {
                "newname": {
                    "format": "int32",
                    "minimum": std::i32::MIN as i64,
                    "maximum": std::i32::MAX as i64,
                },
            },
        })
    );
}

#[test]
fn serde_rename_enum_variant() {
    #[allow(dead_code)]
    #[derive(Serialize, JsonSchema)]
    #[serde(tag = "variant", content = "value")]
    enum Quux {
        #[serde(rename = "LongName")]
        Variant(String),
    }

    assert_eq!(
        Quux::json_schema(),
        json! ({
            "anyOf": [
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["variant", "value"],
                    "properties": {
                        "variant": {
                            "enum": ["LongName"],
                        },
                        "value": {
                            "type": "string",
                        },
                    },
                },
            ],
        })
    );
}

#[test]
fn optional_enum() {
    #[allow(dead_code)]
    #[derive(Serialize, JsonSchema)]
    enum Value {
        Val(String),
    }

    assert_eq!(
        Option::<Value>::json_schema(),
        json! ({
            "anyOf": [
                {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["Val"],
                    "properties": {
                        "Val": {
                            "type": "string"
                        },
                    },
                },
                {
                    "type": "null"
                },
            ]
        })
    );
}

#[test]
fn std_ranges() {
    use std::i32;
    use std::ops::{Range, RangeInclusive};

    #[allow(dead_code)]
    #[derive(JsonSchema)]
    struct Ranges {
        half_open: Range<i32>,
        closed: RangeInclusive<f64>,
    }

    assert_eq!(
        Ranges::json_schema(),
        json! ({
            "type": "object",
            "additionalProperties": false,
            "required": ["half_open", "closed"],
            "properties": {
                "half_open": {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["start", "end"],
                    "properties": {
                        "start": {
                            "format": "int32",
                            "minimum": i32::MIN as i64,
                            "maximum": i32::MAX as i64,
                        },
                        "end": {
                            "format": "int32",
                            "minimum": i32::MIN as i64,
                            "maximum": i32::MAX as i64,
                        },
                    }
                },
                "closed": {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["start", "end"],
                    "properties": {
                        "start": {
                            "type": "number",
                            "format": "double",
                            "minimum": std::f64::MIN,
                            "maximum": std::f64::MAX
                        },
                        "end": {
                            "type": "number",
                            "format": "double",
                            "minimum": std::f64::MIN,
                            "maximum": std::f64::MAX
                        },
                    }
                },
            }
        })
    );
}

#[test]
fn std_sequence_collections() {
    use std::collections::{BinaryHeap, LinkedList, VecDeque};

    #[allow(dead_code)]
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, JsonSchema)]
    enum ElaborateType {
        Foo(Option<u64>),
        Bar { field: String },
        Qux(Box<[bool; 4]>),
    }

    let array_schema = json! ({
        "type": "array",
        "items": ElaborateType::json_schema(),
    });

    assert_eq!(Vec::<ElaborateType>::json_schema(), array_schema);
    assert_eq!(VecDeque::<ElaborateType>::json_schema(), array_schema);
    assert_eq!(BinaryHeap::<ElaborateType>::json_schema(), array_schema);
    assert_eq!(LinkedList::<ElaborateType>::json_schema(), array_schema);
}
