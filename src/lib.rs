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

#[derive(PartialEq, Debug, Copy, Clone)]
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

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum VarType {
    Wire,
    Reg,
    TriReg,
    Integer,
    Port,
}

impl TryFrom<u8> for VarType {
    type Error = &'static str;

    fn try_from(data: u8) -> Result<Self, Self::Error> {
        match data {
            0 => Ok(VarType::Wire),
            1 => Ok(VarType::Reg),
            2 => Ok(VarType::TriReg),
            3 => Ok(VarType::Integer),
            4 => Ok(VarType::Port),
            _ => Err("bad encoded variable type"),
        }
    }
}

impl From<VarType> for u8 {
    fn from(data: VarType) -> Self {
        let exp: u8 = match data {
            VarType::Wire => 0,
            VarType::Reg => 1,
            VarType::TriReg => 2,
            VarType::Integer => 3,
            VarType::Port => 4,
        };
        exp
    }
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

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum ScopeType {
    Module,
    Task,
    Function,
    Fork,
}

impl TryFrom<u8> for ScopeType {
    type Error = &'static str;

    fn try_from(data: u8) -> Result<Self, Self::Error> {
        match data {
            0 => Ok(ScopeType::Module),
            1 => Ok(ScopeType::Task),
            2 => Ok(ScopeType::Function),
            3 => Ok(ScopeType::Fork),
            _ => Err("bad encoded variable type"),
        }
    }
}

impl From<ScopeType> for u8 {
    fn from(data: ScopeType) -> Self {
        let exp: u8 = match data {
            ScopeType::Module => 0,
            ScopeType::Task => 1,
            ScopeType::Function => 2,
            ScopeType::Fork => 3,
        };
        exp
    }
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

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ScalarValue {
    ZeroOne(bool),
    Xstate,
    Zstate,
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ScalarValue::ZeroOne(v) => match &v {
                true => write!(f, "1"),
                false => write!(f, "0"),
            },
            ScalarValue::Xstate => write!(f, "x"),
            ScalarValue::Zstate => write!(f, "z"),
        }
    }
}

impl Default for ScalarValue {
    fn default() -> Self {
        ScalarValue::Xstate
    }
}

impl std::ops::Not for ScalarValue {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            ScalarValue::ZeroOne(b) => ScalarValue::ZeroOne(!b),
            ScalarValue::Xstate => ScalarValue::Xstate,
            ScalarValue::Zstate => ScalarValue::Zstate,
        }
    }
}

impl std::ops::BitXor for ScalarValue {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ScalarValue::ZeroOne(s1), ScalarValue::ZeroOne(s2)) => ScalarValue::ZeroOne(s1 ^ s2),
            (ScalarValue::Xstate, _)
            | (ScalarValue::Zstate, _)
            | (_, ScalarValue::Xstate)
            | (_, ScalarValue::Zstate) => ScalarValue::ZeroOne(false),
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

#[derive(PartialEq, Debug, Clone)]
pub struct Variable {
    pub var_type: VarType,
    pub name: String,
    pub width: u16,
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
    // 1st dimension is timestamp, 2nd dimension is signal
    pub var_value: Vec<Vec<VarValue>>,
    // <variable identifier, variable index> mapping
    pub var_id_map: HashMap<String, VariableIndex>,
    // <variable identifier, value index> mapping
    // pub var_value_map:HashMap<String,ValueIndex>,
    // <value index,variable identifier> mapping
    pub value_var_map: HashMap<ValueIndex, String>,
    // indicate start index of value, because vcd may missing value if no transition happened after a timestamp
    pub padding_value: Vec<Vec<usize>>,
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
            padding_value: vec![],
        }
    }
}

pub fn multi_parse_vcd(file: &str) -> Result<VcdDb, VcdError> {
    let (s, r) = std::sync::mpsc::channel();
    s.send(std::fs::read_to_string(&file).unwrap()).unwrap();

    let vcd: VcdDb = vcd_parser(&r.recv().unwrap())?;
    Ok(vcd)
}

pub fn parse_vcd(file: &str) -> Result<VcdDb, VcdError> {
    // let report = VcdIoReport::new(&file);
    // let buff = std::fs::read_to_string(&file)?;
    let vcd: VcdDb = vcd_parser(file)?;
    Ok(vcd)
}
