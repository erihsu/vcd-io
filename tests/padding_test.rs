use vcd_io::*;

#[test]
fn padding_checker1() {
    let filename = "example1.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            println!("vcd db is {:?}", vcd_db);
            assert_eq!(vcd_db.timescale, (1, TimeUnit::Usec));
            assert_eq!(
                vcd_db.aligned(),
                true
            );            
        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}

#[test]
fn padding_checker2() {
    let filename = "handy.vcd";
    match parse_vcd(&format!(
        "{}/testcases/{}",
        std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        filename
    )) {
        Ok(vcd_db) => {
            println!("vcd db is {:?}", vcd_db);
            assert_eq!(
                vcd_db.aligned(),
                true
            );              
        }
        Err(e) => {
            panic!("{:?}", e);
            // assert!(false);
        }
    }
}

