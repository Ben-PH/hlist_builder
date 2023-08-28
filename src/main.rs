

fn main() {
    let types = vec!["u8".to_string() , "bool".to_string(), "f32".to_string()];
    let gen = gen_where_line(0, 3, &types);
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
fn gen_where_line(list_n: usize, last_post: usize, types: &[String]) -> String {
    // first, set up the "what it must impl
    if list_n == last_post{
        return format!(": {},\n", plucker(&types[last_post - 1], list_n));
    }
    if last_post == 1 {
        return format!("L0{}", gen_where_line(list_n - 1, last_post, types));
    }

    format!("{}{}", cur_type(list_n, types), gen_where_line(list_n + 1, last_post, types))
}

fn cur_type(list_n: usize, types: &[String]) -> String {
    if list_n == 0 {
        return "L0".to_string();
    }
    format!("<{} as {}>::Remainder", cur_type(list_n - 1, types), plucker(&types[list_n - 1], list_n))
}

fn alias(origin: &String, alias: Option<&String>) -> String {
    let Some(alias) = alias else {
        return origin.clone();
    };
    format!("<{} as {}>::Remainder", origin, alias)
}


fn plucker(tp: &String, list_n: usize) -> String {
    format!("Plucker<{}, L{}>", tp, list_n )
}
