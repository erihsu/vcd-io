use vcd_io::*;

#[test]
fn content_checker1() {
    let filename = "example1.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            assert_eq!(vcd_db.timescale, (1, TimeUnit::Usec));
            assert_eq!(vcd_db.variable.len(), 5);
            assert_eq!(
                vcd_db.version,
                "libsigrok 0.6.0-git-c03aaf342c3f".to_string()
            );
            assert_eq!(
                vcd_db.comment,
                "Acquisition with 6/6 channels at 200 kHz".to_string()
            );
            assert_eq!(vcd_db.scope.len(), 1);
            assert_eq!(
                vcd_db.var_value[0],
                vec![
                    VarValue::Scalar(ScalarValue::ZeroOne(true)),
                    VarValue::Scalar(ScalarValue::ZeroOne(false)),
                    VarValue::Scalar(ScalarValue::ZeroOne(false)),
                    VarValue::Scalar(ScalarValue::ZeroOne(true)),
                    VarValue::Vector(vec![
                        ScalarValue::ZeroOne(true),
                        ScalarValue::ZeroOne(true),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(false)
                    ]),
                ]
            );
            assert_eq!(vcd_db.timestap[0], 0);
        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}
