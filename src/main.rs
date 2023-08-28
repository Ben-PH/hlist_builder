use proc_macro2::Span;
use quote::quote;
use syn::{Block, Ident, Stmt};

fn main() {
    let types = vec!["u8", "bool"]; // , "f32", "Foo", "Bar", "Baz"];
    let mut wheres = (0..types.len())
        .map(|i| gen_where_line(("".to_string(), 0), &types[0..=i]))
        .collect::<Vec<_>>();
    wheres.push(next_type(
        &mut (wheres.last().unwrap().clone(), types.len() as u8 + 1),
        None,
    ));
    dbg!(wheres);
}

fn gen_where_line(mut acc: (String, u8), types: &[&'static str]) -> String {
    if types.len() == 0 {
        return acc.0;
    }

    let next = next_type(&mut acc, Some(types[0]));
    gen_where_line((next, acc.1 + 1), &types[1..])
}

fn next_type(curr: &mut (String, u8), next: Option<&'static str>) -> String {
    // not the start: bump it over
    if curr.0.contains(": Plucker") {
        let constraint = next.map_or("".to_string(), |next| {
            format!(": Plucker<{}, L{}>", next.clone(), curr.1 + 1)
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
    format!("L0: Plucker<{}, L1>", next.unwrap())
}
fn gen_stmts(fields: Vec<Ident>, args: Vec<Ident>) -> Block {
    let mut list_n = 0;
    let mut stmts: Vec<Stmt> = vec![];
    // Generate the "let (field, lX) = lY.pluck();" statements
    for id in &fields {
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

fn gen_block_lines<'a>(mut fields: Vec<String>, mut args: Vec<String>) -> Vec<String> {
    let mut list_n = 0;
    let mut res: Vec<String> = vec![];
    for id in fields.iter() {
        res.push(format!(
            "let ({}, l{}) = l{}.pluck();",
            id,
            list_n + 1,
            list_n
        ));
        list_n += 1;
    }

    res.append(&mut vec!["(".to_string(), "Self {".to_string()]);
    fields.append(&mut args);
    let fields = fields.join(",\n");
    res.push(fields);
    res.push("\n},".to_string());
    res.push(format!("l{}", list_n));
    res.push("\n".to_string());
    res.push(")".to_string());
    res
}
