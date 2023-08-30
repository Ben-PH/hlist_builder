extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Block, DeriveInput, FnArg,
    GenericParam, Generics, Ident, Pat, PatIdent, PatType, Stmt, Type, WherePredicate, TypeParamBound, PathSegment, Path, GenericArgument, TraitBoundModifier, TraitBound,
};


struct ArgPair {
    ident: syn::Ident,
    tp: syn::Type
}

impl ArgPair {
    /// ```
    /// #[hl_build]
    /// struct Foo {
    ///   [hl_field]
    ///   foo: u8,
    /// }
    /// // -> `foo: u8` to pass into a function argument
    fn make_args(fields: Vec<ArgPair>) -> impl Iterator<Item = FnArg> {
       std::iter::once(syn::parse2(quote!{l0: L0}).unwrap()).chain(fields.into_iter().map(FnArg::from))
    }
}
impl From<(syn::Ident, syn::Type)> for ArgPair {
    fn from(value: (syn::Ident, syn::Type)) -> Self {
        Self{ ident: value.0, tp: value.1 }
    }
}
impl From<ArgPair> for syn::FnArg {
    fn from(value: ArgPair) -> Self {
        syn::FnArg::Typed(syn::PatType {
            attrs: Vec::new(),
            pat: Box::new(syn::Pat::Ident(syn::PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident: value.ident.clone(),
                subpat: None,
            })),
            colon_token: syn::token::Colon { spans: [proc_macro2::Span::call_site()] },
            ty: Box::new(value.tp.clone())
        })
    }
}

#[derive(Clone)]
struct WhereLine {
    tp: syn::Type,
    pred: PluckParam
}

impl std::fmt::Debug for WhereLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tp = &self.tp;
        let pred = &self.pred.0;
        let tp_tokens = quote! { #tp };
        let pred_tokens = quote! { #pred };

        f.debug_struct("WhereLine")
         .field("tp", &tp_tokens)
         .field("pred", &pred_tokens)
         .finish()
    }
}
impl WhereLine {
    fn gen_lines_top(types: &[syn::Type]) -> Vec<Self> {
        if types.len() == 0 {
            panic!("no pluckings happening");
        }
        let base = Self::gen_base(&types[0]);
        if types.len() == 1 {
            return vec![base];
        }
        Self::gen_lines_recur(vec![base],  &types[1..])
    }

    fn gen_lines_recur(mut acc: Vec<Self>, types: &[syn::Type]) -> Vec<Self> {
        // use the previous predicate to make the new type
        let tp = acc.last().cloned().expect("should never recurse without the base...").absorb();

        let pred = PluckParam::from((types[0].clone(), acc.len() as u8 + 1));
        acc.push(Self{tp, pred});
        if types.len() == 1 {
            return acc;
        }
        Self::gen_lines_recur(acc,  &types[1..])
    }
    /// L0: Plucker<tp, L1>
    fn gen_base(tp: &syn::Type) -> Self {
        let pred = syn::parse2( quote!{Plucker<#tp, L1>} ).expect("quote the base plucker");
        // Create the WhereLine
        WhereLine {
            tp: syn::parse2( quote!{L0} ).expect("quote the L0"),
            pred: PluckParam(pred),
        }
    }
    /// Tn: Pn -> <Tn as Pn>::Remainder
    fn absorb(self) -> syn::Type {
        let WhereLine { tp, pred } = self;
        let pred = pred.0;
        
        let res = quote!{<#tp as #pred>::Remainder};
        syn::parse2(res).expect("absorbing")
    }
}

impl From<WhereLine> for WherePredicate {
    fn from(line: WhereLine) -> Self {
        let WhereLine { tp, pred } = line;
        let bound: TypeParamBound = pred.0.into();
        let predicate = quote! { #tp: #bound };
        syn::parse2(predicate).expect("whereline to pred")
    }
}

struct PredicateVec {
    preds: Vec<WherePredicate>
}
impl From<Vec<WhereLine>> for PredicateVec {
    fn from(value: Vec<WhereLine>) -> Self {
        Self{ preds: value.into_iter().map(WherePredicate::from).collect() }
    }
}
impl From<PredicateVec> for syn::WhereClause {
    fn from(value: PredicateVec) -> Self {
        Self {
            where_token: syn::Token![where](proc_macro2::Span::call_site()),
            predicates: syn::punctuated::Punctuated::from_iter(value.preds),
        }
    }
}

/// makes Plucker<Tp, LN>

#[derive(Clone)]
struct PluckParam(syn::TypeParamBound);
impl From<(syn::Type, u8)> for PluckParam {
    fn from((ty, n): (syn::Type, u8)) -> Self {
 // Create an ident for "Plucker"
        let plucker_ident = syn::Ident::new("Plucker", Span::call_site());

        // Create an ident for "LN" where N is the u8 value
        let l_ident = syn::Ident::new(&format!("L{}", n), Span::call_site());

        // Construct the inner Path for Plucker<type, LN>
        let inner_path = Path {
            leading_colon: None,
            segments: {
                let mut segments = syn::punctuated::Punctuated::new();
                segments.push_value(PathSegment {
                    ident: plucker_ident,
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            colon2_token: None,
                            lt_token: syn::Token![<](Span::call_site()),
                            args: {
                                let mut args = syn::punctuated::Punctuated::new();
                                args.push_value(syn::GenericArgument::Type(ty));
                                args.push_punct(syn::Token![,](Span::call_site()));
                                args.push_value(syn::GenericArgument::Type(
                                    Type::Path(syn::TypePath {
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: {
                                                let mut segments = syn::punctuated::Punctuated::new();
                                                segments.push_value(PathSegment {
                                                    ident: l_ident,
                                                    arguments: syn::PathArguments::None,
                                                });
                                                segments
                                            },
                                        },
                                    }),
                                ));
                                args
                            },
                            gt_token: syn::Token![>](Span::call_site()),
                        },
                    ),
                });
                segments
            },
        };

        // Construct the Plucker<type, LN> as a TypeParamBound::Trait
        let pluck_param_bound = TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: inner_path,
        });

        PluckParam(pluck_param_bound)
    }
}

#[proc_macro_attribute]
pub fn hl_build(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    // get the fields: the list-built fields, and the manually-built fields
    let (input, annotated_fields, non_annotated_fields) = parse_fields(input);

    // let (list_fieldn, ln+1) = ln.pluck();
    // ...also set the non-list built fields (from fn args)
    let block = gen_stmts(
        &annotated_fields.iter().map(|ArgPair{ident, ..}| ident.clone()).collect(),
        &non_annotated_fields
            .iter()
            .map(|ArgPair{ident, ..}| ident.clone())
            .collect::<Vec<_>>()[..],
    );

    // hl_new args include the injected list, and values for the non-list built args
    let args = ArgPair::make_args(non_annotated_fields);

    if annotated_fields.len() == 0 {
        panic!("redundant builder annotations");
    }
    let types = annotated_fields.iter().map(|ArgPair{ tp, .. }| tp.clone()).collect::<Vec<_>>();

    // make all where-clauses
    let lines: Vec<WhereLine> = WhereLine::gen_lines_top(&types[..]);

    // Take the last clause and absorb that to make the ret-val
    let ret = WhereLine::absorb(lines.last().expect("last line").clone());



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
                punctuated.push(ret);
                punctuated
            },
        })),
    );
    // tie all the signature details together
    let sig = syn::Signature {
        ident: fn_ident,
        generics: Generics {
            where_clause: Some(PredicateVec::from(lines).into()),
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
    let fun = syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        sig,
        block: Box::new(block),
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
    Vec<ArgPair>,
    Vec<ArgPair>,
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
                annotated_fields.push((field_ident, field_type).into());
                // Remove the hl_field attribute
                field.attrs.retain(|attr| !attr.path().is_ident("hl_field"));
            } else {
                non_annotated_fields.push((field_ident, field_type).into());
            }
        }
    }
    (input, annotated_fields, non_annotated_fields)
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
