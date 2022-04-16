use thiserror::Error;


#[derive(Debug,Error)]
pub enum VcdError {
	#[error("Cannot parse the given *.vcd")]
	BadVCD,
	#[error("IO error while opening vcd file")]
	InvalidPath {
		#[from]
		source: std::io::Error,
	}
}
