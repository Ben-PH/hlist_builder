use frunk::hlist::Plucker;

#[hl_build_macro::hl_build]
struct Blinker {
    #[hl_field]
    field0: u8,
    #[hl_field]
    field1: bool,
    field3: f32,
}

// impl Blinker {
//     fn hl_new<L0, L1, L2>(
//         l0: L0,
//         field3: f32,
//     ) -> (
//         Blinker,
//         <<L0 as Plucker<u8, L1>>::Remainder as Plucker<bool, L2>>::Remainder,
//     )
//     where
//         L0: Plucker<u8, L1>,
//         <L0 as Plucker<u8, L1>>::Remainder: Plucker<bool, L2>,
//     {
//         let (pin4, l1) = l0.pluck();
//         let (pin5, l2) = l1.pluck();
//         (
//             Blinker {
//                 field0: pin4,
//                 field1: pin5,
//                 field3,
//             },
//             l2,
//         )
//     }
// }

fn foo() {
    let list = frunk::hlist!(true, 3u8, String::from("list-str"), 10.4);
    let (blinker, list) = Blinker::hl_new(list, 69.420);
}
