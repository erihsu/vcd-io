use vcd_io::parse_vcd;

macro_rules! vcd_picker {
    ($file:literal) => {
        if let Err(e) = parse_vcd(&format!(
            "{}/testcases/{}",
            std::env::var("CARGO_MANIFEST_DIR").unwrap(),
            $file
        )) {
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
fn parser_test1() {
    vcd_picker!("CLA.vcd");
}

#[test]
fn parser_test2() {
    vcd_picker!("example1.vcd");
}

#[test]
fn parser_test3() {
    vcd_picker!("example2.vcd");
}

#[test]
fn parser_test4() {
    vcd_picker!("example2a.vcd");
}

#[test]
fn parser_test5() {
    vcd_picker!("example3.vcd");
}

#[test]
fn parser_test8() {
    vcd_picker!("IEEE_std_example.vcd");
}

#[test]
fn parser_test9() {
    vcd_picker!("wire_types.vcd");
}
