#![allow(dead_code)]
// const VCD_KEYWORDS: [&str;12] =
// 			["$commet","$date","$end","$timescale","$var",
// 			"$upscope","$enddefinitions","$dumpvars","$version","$dumpall",
// 			"$scope","dumpoff","dumpon"];
use serde::{Serialize, Deserialize};

mod error;
mod parser;
mod saver;

use parser::vcd_parser;

use crate::error::VcdError;

#[derive(PartialEq, Debug, Copy, Clone,Serialize, Deserialize,Default)]
pub enum TimeUnit {
    Psec,
    #[default]
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

#[derive(PartialEq, Debug, Copy, Clone,Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Copy, Clone,Serialize,Deserialize)]
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

#[cfg(feature = "dev")]
use rand::{
    distributions::{Distribution, Standard},
    Rng,
};

#[cfg(feature = "dev")]
impl Distribution<ScalarValue> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> ScalarValue {
        match rng.gen_range(0..=3) {
            0 => ScalarValue::ZeroOne(true),
            1 => ScalarValue::ZeroOne(false),
            2 => ScalarValue::Xstate,
            3 => ScalarValue::Zstate,
            _ => ScalarValue::ZeroOne(false),
        }
    }
}

#[cfg(feature = "dev")]
impl Distribution<VarType> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> VarType {
        match rng.gen_range(0..=3) {
            0 => VarType::Port,
            1 => VarType::Reg,
            2 => VarType::TriReg,
            3 => VarType::Integer,
            _ => VarType::Wire,
        }
    }

    // other methods in Distribution are derived from `sample`
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
impl VarValue {
    pub fn padding(&mut self,target_width:usize) -> bool {
        if let VarValue::Vector(ref mut vec) = self {
            if target_width < vec.len() {
                return false;
            }
            let remain_bits = target_width - vec.len();
            if vec.len() == 1 {
                if vec[0] != ScalarValue::ZeroOne(true) {
                    (0..remain_bits).into_iter().for_each(|_|vec.push(vec[0]));
                } else {
                    (0..remain_bits).into_iter().for_each(|_|vec.push(ScalarValue::ZeroOne(false)));
                }
            } else {
                (0..remain_bits).into_iter().for_each(|_|vec.push(ScalarValue::ZeroOne(false)));
            }
        }
        true
    }
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

#[derive(PartialEq, Debug, Clone,Serialize, Deserialize)]
pub struct Variable {
    pub var_type: VarType,
    pub name: String,
    pub width: u16,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub scope_name: String,
    pub sub_scope_idx: Vec<ScopeIndex>,
    pub variables: Vec<VariableIndex>,
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

    pub fn aligned(&self) -> bool {
        let var_num = self.variable.len();
        for val in &self.var_value {
            if val.len() != var_num {
                return false;
            }
        }
        return true;
    }

    // result: 
    // 1. clean data in padding value and align data in var_value
    // 2. clean data in value_var_map
    pub fn align_var_value(&mut self) -> bool {
        let var_num = self.variable.len();
        let mut prev_cycle_data = self.var_value[0].clone();
        let mut curr_cycle_data = prev_cycle_data.clone();
        println!("total var num {:?}", curr_cycle_data.len());
        let mut mask:Vec<bool> = std::iter::repeat(false).take(var_num).collect();
        let mut internal_cursor = 0;

        for (i,padding) in self.padding_value[1..].iter().enumerate() {
            if padding.len() + self.var_value[i+1].len() != var_num {
                // broken data
                return false;
            }
            for p in padding {
                mask[*p] = true;
            }
            mask.iter().enumerate().for_each(|(j,is_masked)|{
                if *is_masked {
                    curr_cycle_data[j] = prev_cycle_data[j].clone();
                } else {
                    curr_cycle_data[j] = self.var_value[i+1][internal_cursor].clone();
                    internal_cursor += 1;
                }
            });

            self.var_value[i+1] = curr_cycle_data.clone();
            prev_cycle_data = curr_cycle_data.clone();
            internal_cursor = 0;
        }

        self.padding_value.clear();
        true
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
