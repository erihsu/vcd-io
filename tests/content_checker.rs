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

#[test]
fn content_checker2() {
    let filename = "example2.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            assert_eq!(vcd_db.comment, "Csum: 1 9dbd5495f6f474ca".to_string());
            assert_eq!(vcd_db.date, "Fri Jul 16 15:30:13 2021".to_string());
            assert_eq!(
                vcd_db.version,
                "Synopsys VCS version R-2020.12-SP1_Full64".to_string()
            );
            assert_eq!(vcd_db.var_id_map.get("4"), Some(&25usize));
            assert_eq!(vcd_db.var_id_map.get(","), Some(&15usize));
            assert_eq!(vcd_db.var_id_map.get("2"), Some(&23usize));
            assert_eq!(vcd_db.var_id_map.get("1"), Some(&22usize));
            assert_eq!(vcd_db.var_id_map.get("%"), Some(&11usize));
        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}

#[test]
fn content_checker3() {
    let filename = "handy.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            assert_eq!(
                vcd_db.var_value[3][0],
                VarValue::Scalar(ScalarValue::ZeroOne(false))
            );
            assert_eq!(vcd_db.timestap.len(), 21);
            assert_eq!(vcd_db.var_value.len(), 21);            
        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}

#[test]
fn content_checker4() {
    let filename = "handy2.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            assert_eq!(vcd_db.date, "Fri Jul 16 15:30:13 2021".to_string());
            assert_eq!(
                vcd_db.version,
                "Synopsys VCS version R-2020.12-SP1_Full64".to_string()
            );

        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}

#[test]
fn content_checker5() {
    let filename = "IEEE_std_example.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            assert_eq!(vcd_db.date, "June 26, 1989 10:05:41".to_string());
            assert_eq!(vcd_db.comment,"".to_string());
            println!("var value {:?}", vcd_db.var_value);
            assert_eq!(vcd_db.variable.len(),5);
            assert_eq!(vcd_db.var_value[0][4],VarValue::Vector(vec![ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,
                                                                    ScalarValue::Xstate,ScalarValue::Xstate,                                                                    
                                                                    ScalarValue::Xstate,ScalarValue::Xstate]))
        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}
