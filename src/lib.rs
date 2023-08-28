use frunk::hlist::Plucker;

#[hl_build_macro::hl_build]
struct Blinker {
    #[hl_field]
    field0: u8,
    #[hl_field]
    field1: bool,
    field2: f32,
}

fn foo() {
    let list = frunk::hlist!(true, 3u8, String::from("list-str"), 10.4);
    let (blinker, list) = Blinker::hl_new(list, 69.420);
}
