use frunk::hlist;



// #[hl_build_macro::hl_build]
// struct Blinker {
// 
//     #[hl_field]
//     pin4: u8,
// 
//     #[hl_field]
//     pin5: u8,
// }
// 
// fn foo() {
//     let list = hlist!(3u8, true, String::from("list-str"), 10.4);
//     let builder = BlinkerBuilder::new(list);
//     let builder = builder.set_pin4();
//     let builder = builder.set_pin5();
//     let (built, new_list) = builder.build();
// }
