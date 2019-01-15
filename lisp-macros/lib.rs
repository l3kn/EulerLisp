extern crate proc_macro;

use proc_macro::TokenStream;
#[macro_use]
extern crate quote;

mod function;

// Passes the attribute attributes and the code after it
// to the macro
#[proc_macro_attribute]
pub fn lisp_fn(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let fn_item: syn::Item = syn::parse(fn_ts.clone()).unwrap();
    let function = function::parse(&fn_item).unwrap();
    println!("{:#?}", function);

    return TokenStream::new();
}
