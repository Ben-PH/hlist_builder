

fn main() {
    let types = vec!["u8", "bool" , "f32", "Foo", "Bar", "Baz"];
    let gen = gen_where_line(("".to_string(), 0), &types);
    dbg!(gen);
}

/*
L0: Plucker<GpioPin<Unknown, 4>, L1>,
<L0 as Plucker<u8, L1>>::Remainder: Plucker<GpioPin<Unknown, 5>, L2>,

<<L0 as Plucker<GpioPin<Unknown, 4>, L1>>::Remainder as Plucker<GpioPin<Unknown, 5>, L2>>::Remainder: Plucker<GpioPin<Unknown, 6>, L3>,
3, 3
: Plucker<GpioPin<Unknown, 6>, L3>,
2, 3: constraint handled
<<L0 as Plucker<GpioPin<Unknown, 4>, L1>>::Remainder as Plucker<GpioPin<Unknown, 5>, L2>>::Remainder
L0 (as)
Plucker<fst, L1>
^^::Remain (as)
Plucker<snd, L2>
^^::Remain (as)
Plucker<thd, L3>
^^::Remainder - for the result

 * */

fn gen_where_line(mut acc: ( String, u8 ), types: &[&'static str]) -> String {
    if types.len() == 0 {
        return acc.0;
    }

    let next = next_type(&mut acc, types[0]);
    gen_where_line((next, acc.1 + 1), &types[1..])

}

fn next_type(curr: &mut (String, u8), next: &'static str) -> String {
    // not the start: bump it over
    if curr.0.contains(": Plucker") {
        return format!("<{}>::Remainder: Plucker<{}, L{}>", curr.0.replace(": Plucker", " as Plucker"), next, curr.1 + 1);
    }

    // start, so load the first type
    format!("L0: Plucker<{}, L1>", next) 
    
}
    
