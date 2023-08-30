use frunk::{hlist, HList};

#[derive(Debug, PartialEq, hl_build_macro::ListBuild)]
pub struct ReferenceStruct {
    field0: u8,
    field1: bool,
}

pub fn demo_use() {
    let list = frunk::hlist!(60u16, 420i16, true, 3u8, String::from("list-str"), 10.4);
    let (reference, list): (_, HList!(u16, i16, String, f32)) = ReferenceStruct::hl_new(list);
    let hand_written = ReferenceStruct {
        field0: 3,
        field1: true,
    };
    assert_eq!(reference, hand_written);
    assert_eq!(list, hlist!(60, 420, String::from("list-str"), 10.4));
}
