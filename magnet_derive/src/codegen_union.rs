//! For the time being, `JsonSchema` can't be automatically derived for a `union`.

use error::{Error, Result};
use proc_macro2::TokenStream;
use syn::{Attribute, DataUnion};

/// Implements `JsonSchema` for a `union`.
pub fn impl_json_schema_union(_: Vec<Attribute>, _: DataUnion) -> Result<TokenStream> {
    Err(Error::new("`JsonSchema` can't be implemented for unions"))
}
