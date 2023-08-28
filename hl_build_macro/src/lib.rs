extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type};


#[proc_macro_attribute]
pub fn hl_build(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as DeriveInput);
    let struct_ident: &syn::Ident = &input.ident;

    let hl_build_fields = field_vec(&input.data);


    // Modify the `input` to remove `hl_build` attributes from fields
    if let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(named), .. }) = &mut input.data {
        for field in &mut named.named {
            field.attrs.retain(|attr| !attr.path().is_ident("hl_field"));
        }
    }


    todo!();
    //expanded.into()
}

fn field_vec(inner: &syn::Data) -> Vec<(syn::Ident, syn::Type)> {
    let mut res: Vec<(syn::Ident, syn::Type)> = Vec::new();

    if let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(named), .. }) = inner {
        for field in named.named.iter() {
            if field.attrs.iter().any(|attr| attr.path().is_ident("hl_field")) {
                let field_ident = field.ident.clone().unwrap(); // Assuming named fields, not
                                                               // tuple-struct
                let field_type = field.ty.clone(); // Type
                res.push((field_ident, field_type));
            }
        }
    }
    res
}

