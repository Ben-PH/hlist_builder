extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Block, DeriveInput, FnArg,
    GenericParam, Generics, Ident, Pat, PatIdent, PatType, Stmt, Type, TypeParam, WherePredicate,
};

fn parse_fields(
    mut input: DeriveInput,
) -> (
    DeriveInput,
    Vec<(syn::Ident, syn::Type)>,
    Vec<(syn::Ident, syn::Type)>,
) {
    let mut annotated_fields = Vec::new();
    let mut non_annotated_fields = Vec::new();

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
    (input, annotated_fields, non_annotated_fields)
}

fn make_args(fields: Vec<(syn::Ident, syn::Type)>) -> impl Iterator<Item = FnArg> {
    let args = fields.into_iter().map(|(ident, ty)| {
        let pat_id = PatIdent {
            attrs: vec![],
            by_ref: None,
            mutability: None,
            ident,
            subpat: None,
        };
        let pat_tp = PatType {
            attrs: vec![],
            pat: Box::new(Pat::Ident(pat_id)),
            colon_token: Default::default(),
            ty: Box::new(ty),
        };
        FnArg::Typed(pat_tp)
    });

    let fn_arg: syn::FnArg = syn::parse_str("l0: L0").unwrap();
    let args = std::iter::once(fn_arg).chain(args);
    args
}
fn make_generic_params(count: usize) -> Punctuated<GenericParam, Comma> {
    let gens: String = (0..count + 1)
        .map(|i| format!("L{}", i))
        .collect::<Vec<String>>()
        .join(", ");
    let gens = format!("<{}>", gens);
    println!("==================\n{}\n===================", gens);
    syn::parse_str::<Generics>(&gens).unwrap().params
}
#[proc_macro_attribute]
pub fn hl_build(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    let (input, annotated_fields, non_annotated_fields) = parse_fields(input);
    let block = gen_stmts(
        &annotated_fields.iter().map(|(id, _)| id.clone()).collect(),
        &non_annotated_fields
            .iter()
            .map(|(id, _)| id.clone())
            .collect(),
    );
    let args = make_args(non_annotated_fields);

    let wheres = (0..annotated_fields.len())
        .map(|i| gen_where_line(("".to_string(), 0), &annotated_fields[0..=i]))
        .collect::<Vec<_>>();

    let where_preds = wheres
        .iter()
        .map(|s| syn::parse_str::<WherePredicate>(s).unwrap())
        .collect::<Vec<_>>();

    let last = where_preds.last().clone().unwrap();
    let ret = next_type(
        &mut (quote! {#last}.to_string(), annotated_fields.len() as u8 + 1),
        None,
    );
    let where_clause = syn::WhereClause {
        where_token: syn::token::Where {
            span: Span::call_site(),
        },
        predicates: Punctuated::from_iter(where_preds),
    };
    let fn_ident = syn::Ident::new("hl_new", proc_macro2::Span::call_site());
    let output: syn::ReturnType = syn::parse_str(format!("-> (Self, {})", ret).as_str()).unwrap();

    let sig = syn::Signature {
        ident: fn_ident,
        generics: Generics {
            where_clause: Some(where_clause),
            params: make_generic_params(annotated_fields.len()),
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
    let struct_ident = input.clone().ident;
    let block = Box::new(block);
    let fun = syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        sig,
        block,
    };
    let result = quote! {
        #input

        impl #struct_ident {
            #fun
        }
    };
    result.into()
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

fn gen_where_line(mut acc: (String, u8), types: &[(Ident, Type)]) -> String {
    if types.len() == 0 {
        return acc.0;
    }

    let next = next_type(&mut acc, Some(&types[0].1));
    gen_where_line((next, acc.1 + 1), &types[1..])
}

fn next_type(curr: &mut (String, u8), next: Option<&Type>) -> String {
    // not the start: bump it over
    if curr.0.contains(": Plucker") {
        let constraint = next.map_or("".to_string(), |next| {
            let next = next.clone();
            format!(
                ": Plucker<{}, L{}>",
                quote! { #next }.to_string(),
                curr.1 + 1
            )
        });
        return format!(
            "<{}>::Remainder{}",
            // treat this list as what's left after a pluck
            curr.0.replace(": Plucker", " as Plucker"),
            // make sure what's left after the pluck can pluck the next type
            constraint,
        );
    }

    // start, so load the first type
    let next = next.unwrap();
    format!("L0: Plucker<{}, L1>", quote! { #next }.to_string())
}

fn gen_stmts(fields: &Vec<Ident>, args: &Vec<Ident>) -> Block {
    let mut list_n = 0;
    let mut stmts: Vec<Stmt> = vec![];
    // Generate the "let (field, lX) = lY.pluck();" statements
    for id in fields {
        let next_list = list_n + 1;
        let next_list = syn::Ident::new(&format!("l{}", next_list), Span::call_site());
        let list_n_tok = syn::Ident::new(&format!("l{}", list_n), Span::call_site());
        let stmt: Stmt = syn::parse2(quote! {
            let (#id, #next_list) = #list_n_tok.pluck();
        })
        .unwrap();
        stmts.push(stmt);
        list_n += 1;
    }

    // Generate the "Self { fields... }" part of the block
    let all_fields = [&fields[..], &args[..]].concat();
    let list_n_ident = syn::Ident::new(&format!("l{}", list_n), proc_macro2::Span::call_site());
    let self_stmt: Stmt = syn::parse2(quote! {
        return (Self { #(#all_fields,)* }, #list_n_ident);
    })
    .unwrap();
    stmts.push(self_stmt);

    Block {
        stmts,
        brace_token: Default::default(),
    }
}
