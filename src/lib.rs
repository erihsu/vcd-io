// const VCD_KEYWORDS: [&str;12] =
// 			["$commet","$date","$end","$timescale","$var",
// 			"$upscope","$enddefinitions","$dumpvars","$version","$dumpall",
// 			"$scope","dumpoff","dumpon"];
#![allow(dead_code)]
mod error;
mod parser;
mod saver;

use parser::vcd_parser;

use crate::error::VcdError;

#[derive(PartialEq, Debug)]
pub enum TimeUnit {
    Psec,
    Nsec,
    Usec,
    Msec,
    Sec,
}

use std::fmt::{self, Display};

impl Display for TimeUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            TimeUnit::Psec => write!(f, "ps"),
            TimeUnit::Nsec => write!(f, "ns"),
            TimeUnit::Usec => write!(f, "us"),
            TimeUnit::Msec => write!(f, "ms"),
            TimeUnit::Sec => write!(f, "s"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum VarType {
    Wire,
    Reg,
    TriReg,
    Integer,
    Port,
}

impl Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            VarType::Wire => write!(f, "wire"),
            VarType::Reg => write!(f, "reg"),
            VarType::TriReg => write!(f, "trireg"),
            VarType::Integer => write!(f, "integer"),
            VarType::Port => write!(f, "port"),
        }
    }
}

pub enum ScopeType {
    Module,
    Task,
    Function,
    Fork,
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ScopeType::Module => write!(f, "module"),
            ScopeType::Task => write!(f, "task"),
            ScopeType::Function => write!(f, "function"),
            ScopeType::Fork => write!(f, "fork"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ScalarValue {
    ZeroOne(bool),
    Xstate,
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ScalarValue::ZeroOne(v) => match &v {
                true => write!(f, "1"),
                false => write!(f, "0"),
            },
            ScalarValue::Xstate => write!(f, "x"),
        }
    }
}

impl std::ops::Not for ScalarValue {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            ScalarValue::ZeroOne(b) => ScalarValue::ZeroOne(!b),
            ScalarValue::Xstate => ScalarValue::Xstate,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum VarValue {
    Scalar(ScalarValue),
    Vector(Vec<ScalarValue>),
    Real(String), // as we dont know real value type, can be converted later
}

impl Display for VarValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            VarValue::Scalar(s) => write!(f, "{}", s),
            VarValue::Vector(v) => {
                write!(f, "b")?;
                for a_v in v {
                    write!(f, "{}", a_v)?;
                }
                Ok(())
            }
            VarValue::Real(r) => write!(f, "r{}", r),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Variable {
    var_type: VarType,
    name: String,
    width: u16,
}

impl Variable {
    pub fn get_width(&self) -> u16 {
        self.width
    }
}

pub struct Scope {
    scope_type: ScopeType,
    scope_name: String,
    sub_scope_idx: Vec<ScopeIndex>,
    variables: Vec<VariableIndex>,
}

use std::collections::HashMap;

use nom::{error::VerboseError, IResult};

pub type ScopeIndex = usize;
pub type VariableIndex = usize;
pub type ValueIndex = usize;

pub type VcdRes<T, U> = IResult<T, U, VerboseError<T>>;

pub struct VcdDb {
    // VCD header
    pub date: String,
    pub version: String,
    pub comment: String,
    pub timescale: (u32, TimeUnit),

    // list of timestap
    pub timestap: Vec<u32>,
    // list of scope
    pub scope: Vec<Scope>,
    // list of variable
    pub variable: Vec<Variable>,
    pub var_value: Vec<Vec<VarValue>>,
    // <variable identifier, variable index> mapping
    pub var_id_map: HashMap<String, VariableIndex>,
    // <variable identifier, value index> mapping
    // pub var_value_map:HashMap<String,ValueIndex>,
    // <value index,variable identifier> mapping
    pub value_var_map: HashMap<ValueIndex, String>,
}

impl VcdDb {
    pub fn new() -> Self {
        VcdDb::default()
    }
}

impl Default for VcdDb {
    fn default() -> Self {
        VcdDb {
            timescale: (1, TimeUnit::Psec),
            date: String::from(""),
            version: String::from(""),
            comment: String::from(""),
            timestap: vec![],
            scope: vec![],
            variable: vec![],
            var_value: vec![],
            var_id_map: HashMap::new(),
            value_var_map: HashMap::new(),
        }
    }
}

pub fn parse_vcd<P: AsRef<std::path::Path> + std::convert::AsRef<std::ffi::OsStr>>(
    file: P,
) -> Result<VcdDb, VcdError> {
    let path = std::path::Path::new(&file);
    let buff = std::fs::read_to_string(&file)?;
    let vcd: VcdDb = vcd_parser(&buff).map_err(|mut e| {
        if let VcdError::BadVCD(ref mut report) = e {
            report.bad_vcd_file = path.to_path_buf()
        }
        e
    })?;
    Ok(vcd)
}
