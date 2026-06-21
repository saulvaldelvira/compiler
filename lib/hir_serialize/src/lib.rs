#[macro_use]
extern crate json as _json;

mod html;
mod json;

pub use html::hir_print_html;
pub use crate::json::serialize_json;
