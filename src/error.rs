use std::path::PathBuf;

use thiserror::Error;
#[derive(Debug, Error)]
pub enum VcdError {
    #[error("Cannot parse the given *.vcd")]
    BadVCD(BadVCDReport),
    #[error("IO error while opening vcd file")]
    InvalidPath {
        #[from]
        source: std::io::Error,
    },
}

#[derive(Debug, Default)]
pub struct BadVCDReport {
    // pub bad_vcd_file: PathBuf,
    pub recovered_buff: String,
    pub error_start_line: u64,
    // pub error_start_column: u16,
    pub possible_error: String,
}

impl BadVCDReport {
    pub fn new() -> Self {
        BadVCDReport::default()
    }
}

pub struct VcdIoReport {
    pub content: BadVCDReport,
    pub file: PathBuf,
}

impl VcdIoReport {
    pub fn new(file: PathBuf) -> Self {
        Self {
            file,
            content: BadVCDReport::default(),
        }
    }
}
