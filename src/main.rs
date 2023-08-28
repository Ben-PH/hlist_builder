fn main() {
    let types = vec!["u8", "bool", "f32", "Foo", "Bar", "Baz"];
    let wheres = (0..types.len()).map(|i| {

        gen_where_line(("".to_string(), 0), &types[0..=i])
    }).collect::<Vec<_>>();
    dbg!(wheres);
}

fn gen_where_line(mut acc: (String, u8), types: &[&'static str]) -> String {
    if types.len() == 0 {
        return acc.0;
    }

    let next = next_type(&mut acc, types[0]);
    gen_where_line((next, acc.1 + 1), &types[1..])
}

fn next_type(curr: &mut (String, u8), next: &'static str) -> String {
    // not the start: bump it over
    if curr.0.contains(": Plucker") {
        return format!(
            "<{}>::Remainder: Plucker<{}, L{}>",
            // treat this list as what's left after a pluck
            curr.0.replace(": Plucker", " as Plucker"),
            // make sure what's left after the pluck can pluck the next type
            next,
            // ...and the resulting list following said pluck, is a known-list
            curr.1 + 1
        );
    }

    // start, so load the first type
    format!("L0: Plucker<{}, L1>", next)
}
