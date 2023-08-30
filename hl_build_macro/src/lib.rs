extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Block, DeriveInput, FnArg,
    GenericParam, Generics, Ident, Pat, PatIdent, PatType, Stmt, Type, WherePredicate, TypeParamBound, PathSegment, Path, GenericArgument, TraitBoundModifier, TraitBound,
};

#[proc_macro_attribute]
pub fn hl_build(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    // get the fields: the list-built fields, and the manually-built fields
    let (input, annotated_fields, non_annotated_fields) = parse_fields(input);

    // let (list_fieldn, ln+1) = ln.pluck();
    // ...also set the non-list built fields (from fn args)
    let block = gen_stmts(
        &annotated_fields.iter().map(|(id, _)| id.clone()).collect(),
        &non_annotated_fields
            .iter()
            .map(|(id, _)| id.clone())
            .collect::<Vec<_>>()[..],
    );

    // hl_new args include the injected list, and values for the non-list built args
    let args = make_args(non_annotated_fields);

    // constrain each resulting list to implement the next plucking
    let types = annotated_fields.iter().cloned().map(|f| f.1);
            dbg!(quote!{#(#types)*});
    let wheres: Vec<WherePredicate> = (0..annotated_fields.len())
        .fold(Vec::new(), |mut acc, i| {
            let curr_pred = acc.last().map(|last| last.clone());
            
            if let Some(new_pred) = gen_where_predicate_entry(curr_pred, 0, &annotated_fields[0..=i]) {
                acc.push(new_pred);
            }
            acc
        });

    dbg!(annotated_fields.len());
    let ret_type = absorb_param_bound(wheres.last().cloned().expect("last wheres")).expect("top-lvl absorb");

    let fn_ident = syn::Ident::new("hl_new", proc_macro2::Span::call_site());
    let output: syn::ReturnType = syn::ReturnType::Type(
        syn::token::RArrow::default(),
        Box::new(syn::Type::Tuple(syn::TypeTuple {
            paren_token: syn::token::Paren::default(),
            elems: {
                let mut punctuated = Punctuated::new();
                punctuated.push(syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::parse_str("Self").expect("parseing Self"),
                }));
                punctuated.push(ret_type);
                punctuated
            },
        })),
    );
    let where_clause = {
        let mut wc = syn::WhereClause {
            where_token: Default::default(),
            predicates: Punctuated::new(),
        };
        for where_predicate in wheres.iter() {
            wc.predicates.push(where_predicate.clone());
        }
        wc
    };
    // tie all the signature details together
    let sig = syn::Signature {
        ident: fn_ident,
        generics: Generics {
            where_clause: Some(where_clause),
            params: make_generic_params(annotated_fields.len()),
            ..Default::default()
        },
        inputs: Punctuated::from_iter(args),

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

    // et, voile! en a des code magnifique!
    quote! {
        #input

        impl #struct_ident {
            #fun
        }
    }.into()
}
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

            let field_ident = field.ident.clone().expect("field ident"); // Assuming named fields
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

/// ```
/// #[hl_build]
/// struct Foo {
///   [hl_field]
///   foo: u8,
/// }
/// // -> `foo: u8` to pass into a function argument
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

    let fn_arg: syn::FnArg = syn::parse_str("l0: L0").expect("parsing fn arg");
    std::iter::once(fn_arg).chain(args)
}

// `<L0, L1, ..., L(N-1)>` for the `fn hl_new<L1, ...>`
fn make_generic_params(count: usize) -> Punctuated<GenericParam, Comma> {
    let gens: String = (0..count + 1)
        .map(|i| format!("L{}", i))
        .collect::<Vec<String>>()
        .join(", ");
    let gens = format!("<{}>", gens);
    println!("==================\n{}\n===================", gens);
    syn::parse_str::<Generics>(&gens).expect("parsing the make_generic_params").params
}


fn gen_where_predicate_entry(curr_pred: WherePredicate, curr_l: u8, types: &[(Ident, Type)]) -> Result<WherePredicate, ()> {
    if types.is_empty() {
        return Err(());
    }
    let top = base_where_pred(&types[0].1);
    Ok(gen_where_predicate_recurse(1, types[1..]))


    // get the needed impl
    let implements = next_type(curr_pred.clone(), curr_l, Some(&types[0].1));
    // make the <curr as plucker>::Remainder
    let absorbed_pred = absorb_param_bound(curr_pred.clone().unwrap());
    Ok(gen_where_predicate_entry(curr_pred, curr_l + 1, &types[1..]))
}
fn gen_where_predicate_recurse(curr_pred: WherePredicate, curr_l: u8, types: &[(Ident, Type)]) -> Result<WherePredicate, ()> {
    let new_type: syn::Type = absorb_param_bound(curr_pred);
    let new_plucker: TypeParamBound = gen_plucker(&types[0].1, curr_l);
    let pred_res = syn::PredicateType {
        lifetimes: None,
        bounded_ty: new_type,
        colon_token: syn::token::Colon { spans: [Span::call_site()] },
        bounds: {
            let mut punctuated = Punctuated::new();
            punctuated.push(new_plucker);
            punctuated
        },
    };
    if types.len() == 1  {
        Ok(pred_res)
    } else {
        gen_where_predicate_recurse(pred_res, curr_l + 1, types[1..]);
}


fn next_type(curr_pred: Option<WherePredicate>, curr_ln: u8, next: Option<&Type>) -> (Type, Option<TypeParamBound>) {
    // base-case. generate the opening where-clause
    let Some(curr_pred) = curr_pred else {
        return base_where_pred(next.expect("return of next-type"));
    };


    let tp_param_bound = next.map(|tp| gen_plucker(tp, curr_ln + 1));
    let absorbed_pred = absorb_param_bound(curr_pred).expect("next-types absorb-param-pound call");

    (absorbed_pred, tp_param_bound)
}

// Foo: Bar -> <Foo as Bar>::Remainder
fn absorb_param_bound(current: WherePredicate) -> Type {
    let WherePredicate::Type(syn::PredicateType { bounded_ty, .. }) = &current else {
        return None;
    };
     let remainder = PathSegment {
            ident: syn::Ident::new("Remainder", proc_macro2::Span::call_site()),
            arguments: syn::PathArguments::None,
        };
        
        // Create path for `as CurrentBound`
        let as_current_bound = Path {
            leading_colon: None,
            segments: vec![remainder].into_iter().collect(),
        };
        
        // Construct the new type path `<CurrentType as CurrentBound>::Remainder`
        let new_type = Type::Path(
            syn::TypePath {
                qself: Some(syn::QSelf {
                    lt_token: syn::token::Lt([proc_macro2::Span::call_site()]),
                    ty: Box::new(bounded_ty.clone()),
                    position: 1,
                    as_token: Some(syn::token::As::default()),
                    gt_token: syn::token::Gt([proc_macro2::Span::call_site()]),
                }),
                path: as_current_bound,
            }
        );
        
        Some(new_type)
}
// `frunk_core::hlist::Plucker<next, Lnext_ln>`
fn gen_plucker(next: &Type, next_ln: u8) -> TypeParamBound {
    let trait_path = Path {
        leading_colon: None,
        segments: vec![
            Ident::new("frunk_core", Span::call_site()),
            Ident::new("hlist", Span::call_site()),
            Ident::new("Plucker", Span::call_site()),
        ].into_iter().map(|ident| PathSegment {
            ident,
            arguments: Default::default(),
        }).collect(),
    };

    let next_generic_argument = GenericArgument::Type(next.clone());

    let l_generic_argument = GenericArgument::Type(Type::Path(syn::TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: vec![
                PathSegment {
                    ident: Ident::new(&format!("L{}", next_ln), Span::call_site()),
                    arguments: Default::default(),
                },
            ].into_iter().collect(),
        },
    }));

    let mut tb = TypeParamBound::Trait(syn::TraitBound {
        path: trait_path,
        modifier: syn::TraitBoundModifier::None,
        paren_token: None,
        lifetimes: None,
    });

    if let TypeParamBound::Trait(ref mut trait_bound) = tb {
        trait_bound.path.segments.last_mut().expect("bad last-mut").arguments = syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            args: vec![next_generic_argument, l_generic_argument].into_iter().collect(),
            colon2_token: None,  // Optional and not needed here
            lt_token: syn::Token![<](Span::call_site()),
            gt_token: syn::Token![>](Span::call_site()),
        });
    }

    tb
}
fn base_where_pred(tp: &Type) -> (Type, Option<TypeParamBound>) {
    // Create the type (i.e., `L0`)
    let bounded_ty = Type::Path(syn::TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: vec![
                PathSegment {
                    ident: Ident::new("L0", Span::call_site()),
                    arguments: Default::default(),
                },
            ].into_iter().collect(),
        },
    });

    let trait_bound_with_generics = gen_plucker(tp, 1);

    (bounded_ty, Some(trait_bound_with_generics))
}
fn gen_stmts(fields: &Vec<Ident>, args: &[Ident]) -> Block {
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
        .expect("");
        stmts.push(stmt);
        list_n += 1;
    }

    // Generate the "Self { fields... }" part of the block
    let all_fields = [&fields[..], args].concat();
    let list_n_ident = syn::Ident::new(&format!("l{}", list_n), proc_macro2::Span::call_site());
    let self_stmt: Stmt = syn::parse2(quote! {
        return (Self { #(#all_fields,)* }, #list_n_ident);
    })
    .expect("");
    stmts.push(self_stmt);

    Block {
        stmts,
        brace_token: Default::default(),
    }
}
