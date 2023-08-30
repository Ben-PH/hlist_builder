use frunk::{hlist::Plucker, hlist, HList};

#[derive(Debug, PartialEq)]
#[hl_build_macro::hl_build]
pub struct ReferenceStruct {
    #[hl_field]
    field0: u8,
    #[hl_field]
    field1: bool,
    fielda: u16,
    fieldb: i16,
}

pub fn demo_use() {
    let list = frunk::hlist!(60u16, 420i16, true, 3u8, String::from("list-str"), 10.4);
    let (reference, list): (_, HList!(u16, i16, String, f32)) = ReferenceStruct::hl_new(list, 10, -2);
    let hand_written = ReferenceStruct{ field0: 3, field1: true, fielda: 10, fieldb: -2 };
    assert_eq!(reference, hand_written);
    assert_eq!(list, hlist!(60, 420, String::from("list-str"), 10.4));
}
