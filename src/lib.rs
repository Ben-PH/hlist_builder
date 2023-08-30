use frunk::{hlist::Plucker, HList};

#[derive(Debug)]
#[hl_build_macro::hl_build]
pub struct ReferenceStruct {
    #[hl_field]
    field0: u8,
    #[hl_field]
    field1: bool,
    field2: f32,
    #[hl_field]
    fielda: u16,
    #[hl_field]
    fieldb: i16,
}

pub fn demo_use() {
    let list = frunk::hlist!(60u16, 420i16, true, 3u8, String::from("list-str"), 10.4);
    let (blinker, list): (_, HList!(String, f32)) = ReferenceStruct::hl_new(list, 69.420);
    println!("{:?}", blinker);
    println!("{:?}", list);
}
