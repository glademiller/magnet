//! Code generation for `struct`s.

use codegen_field::impl_json_schema_fields;
use error::Result;
use proc_macro2::TokenStream;
use syn::{Attribute, DataStruct};

/// Implements `JsonSchema` for a `struct`.
pub fn impl_json_schema_struct(attrs: Vec<Attribute>, ast: DataStruct) -> Result<TokenStream> {
    impl_json_schema_fields(&attrs, ast.fields)
}
