extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type};


#[proc_macro_attribute]
pub fn hl_build(_attr: TokenStream, item: TokenStream) -> TokenStream {
    println!("item: \"{}\"", item.to_string());
    let mut input = parse_macro_input!(item as DeriveInput);
    let struct_ident: &syn::Ident = &input.ident;
    let builder_struct_name = format!("{}Builder", &input.ident);
    let builder_struct_ident = syn::Ident::new(&builder_struct_name, struct_ident.span());
    let hl_build_fields = field_vec(&input.data);

    let field_definitions: Vec<proc_macro2::TokenStream> = hl_build_fields.iter().map(|(field_name, field_type)| {
        let field_name = field_name.clone();
        quote! { #field_name: Option<#field_type> }
    }).collect();

    // Modify the `input` to remove `hl_build` attributes from fields
    if let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(named), .. }) = &mut input.data {
        for field in &mut named.named {
            field.attrs.retain(|attr| !attr.path().is_ident("hl_field"));
        }
    }

    let mut setters = vec![];
     for (id, tp) in hl_build_fields.iter(){ 
         setters.push(gen_setter(id, tp, &builder_struct_ident)) 
     }

    let impls = quote!{ 
        #(#setters),* 
    };
    let expanded = quote! {
        #input
        struct #builder_struct_ident<T> {
            list: T,
            #(#field_definitions),*
        }

        
        impl<T> #builder_struct_ident<T> {
            fn new(list: T) -> Self {
                Self {
                    list,
                    pin4: None,
                    pin5: None,
                }
            }

            #impls

            fn build(self) -> ( #struct_ident, T ) {
                (
                    #struct_ident {
                        pin4: self.pin4.unwrap(),
                        pin5: self.pin5.unwrap(),
                    },
                    self.list
                )
            }

        }
    };

    expanded.into()
}

// fn gen_where_clause(depth: u8, types: Vec<Type>) -> String {
//     format!("{}L{}: Plucker<{}, {}>", std::iter::repeat('<').take(depth).collect::<String>(), depth, types[depth])
// }


fn gen_setter(field_ident: &syn::Ident, field_type: &syn::Type, builder_ident: &syn::Ident) -> proc_macro2::TokenStream {
    // set the ident
    let field_name = format!("{}", field_ident);
    let setter_name = format!("set_{}", field_ident);
    let setter_ident = syn::Ident::new(&setter_name, proc_macro2::Span::call_site());

    quote!{
        fn #setter_ident<Remain>(self) -> #builder_ident<T::Remainder>
        where 
            T: frunk::hlist::Plucker<#field_type, Remain>
        {
            let (item, list) = self.list.pluck();
            self.list = list;
            self.#field_ident = Some(item);
            self
        }
    }
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

