use std::fs::File;
use vcd_io::mt_parse_vcd;

macro_rules! mt_vcd_picker {
    ($file:literal) => {
        let file = File::open(&format!(
            "{}/testcases/{}",
            std::env::var("CARGO_MANIFEST_DIR").unwrap(),
            $file
        )).unwrap();
        if let Err(e) = mt_parse_vcd(file) {
            panic!("error {:?}", e);
        }
    };
}

// macro_rules! fast_vcd_picker {
//     ($file:literal) => {
//         if let Err(e) = multi_parse_vcd(&format!(
//             "{}/testcases/{}",
//             std::env::var("CARGO_MANIFEST_DIR").unwrap(),
//             $file
//         )) {
//             panic!("error {:?}", e);
//         }
//     };
// }


#[test]
fn mt_parser_test1() {
    mt_vcd_picker!("CLA.vcd");
}

#[test]
fn mt_parser_test2() {
    mt_vcd_picker!("example1.vcd");
}

#[test]
fn mt_parser_test3() {
    mt_vcd_picker!("example2.vcd");
}

#[test]
fn mt_parser_test4() {
    mt_vcd_picker!("example2a.vcd");
}

#[test]
fn mt_parser_test5() {
    mt_vcd_picker!("example3.vcd");
}

#[test]
fn mt_parser_test8() {
    mt_vcd_picker!("IEEE_std_example.vcd");
}

#[test]
fn mt_parser_test9() {
    mt_vcd_picker!("wire_types.vcd");
}

#[test]
fn mt_parser_test10() {
    mt_vcd_picker!("handy.vcd");
}

#[test]
fn mt_parser_test11() {
    mt_vcd_picker!("handy2.vcd");
}

#[test]
fn mt_parser_test12() {
    mt_vcd_picker!("handy3.vcd");
}

