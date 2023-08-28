extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Block, DeriveInput, FnArg, Generics, Pat, PatIdent, PatType, Type, TypeParam, punctuated::Punctuated,
};

#[proc_macro_attribute]
pub fn hl_build(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as DeriveInput);

    let mut annotated_fields: Vec<(syn::Ident, syn::Type)> = Vec::new();
    let mut non_annotated_fields: Vec<(syn::Ident, syn::Type)> = Vec::new();

    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(named),
        ..
    }) = &mut input.data
    {
        for field in &mut named.named {
            let is_annotated = field
                .attrs
                .iter()
                .any(|attr| attr.path().is_ident("hl_field"));

            let field_ident = field.ident.clone().unwrap(); // Assuming named fields
            let field_type = field.ty.clone(); // Type

            if is_annotated {
                annotated_fields.push((field_ident, field_type));
                // Remove the hl_field attribute
                field.attrs.retain(|attr| !attr.path().is_ident("hl_field"));
            } else {
                non_annotated_fields.push((field_ident, field_type));
            }
        }
    }

    let args = non_annotated_fields
        .iter()
        .map(|(ident, ty)| {
            let pat_id = PatIdent {
                attrs: vec![],
                by_ref: None,
                mutability: None,
                ident: ident.clone(),
                subpat: None,
            };
            let pat_tp = PatType {
                attrs: vec![],
                pat: Box::new(Pat::Ident(pat_id)),
                colon_token: Default::default(),
                ty: Box::new(ty.clone()),
            };
            FnArg::Typed(pat_tp)
        })
        .collect::<Vec<_>>();

    let gens: String = annotated_fields
        .iter()
        .enumerate()
        .map(|(i, _)| format!("L{}", i))
        .collect::<Vec<String>>()
        .join(", ");
    let gens = format!("<{}>", gens);
    let parsed_gens: syn::Generics = syn::parse_str(&gens).unwrap();
    let where_clause: syn::WhereClause = syn::parse_str("where Foo: Bar").unwrap();
    let fn_ident = syn::Ident::new("hl_new", proc_macro2::Span::call_site());
    let output: syn::ReturnType = syn::parse_str("-> (Self, Fizz)").unwrap();
    let fn_arg: syn::FnArg = syn::parse_str("l0: L0").unwrap();
    let args = std::iter::once(fn_arg).chain(args);

    let sig = syn::Signature {
        ident: fn_ident,
        generics: Generics {
            where_clause: Some(where_clause),
            params: parsed_gens.params,
            ..Default::default()
        },
        inputs: Punctuated::from_iter(args.into_iter()),

        output,
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        variadic: None,
        fn_token: Default::default(),
        paren_token: Default::default(),
    };
    let block = Box::new(syn::parse_str::<Block>("{ /* function body */ }").unwrap());
    let fun = syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        sig,
        block,
    };
    dbg!(quote! {#fun}.to_string());
    let mut struct_tokens = proc_macro2::TokenStream::new();
    input.to_tokens(&mut struct_tokens);
    println!("Parsed Struct:\n{}", struct_tokens.to_string());
    todo!();
    //expanded.into()
}

fn field_vec(inner: &syn::Data) -> Vec<(syn::Ident, syn::Type)> {
    let mut res: Vec<(syn::Ident, syn::Type)> = Vec::new();

    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(named),
        ..
    }) = inner
    {
        for field in named.named.iter() {
            if field
                .attrs
                .iter()
                .any(|attr| attr.path().is_ident("hl_field"))
            {
                let field_ident = field.ident.clone().unwrap(); // Assuming named fields, not
                                                                // tuple-struct
                let field_type = field.ty.clone(); // Type
                res.push((field_ident, field_type));
            }
        }
    }
    res
}
