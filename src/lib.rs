#![allow(dead_code)]
// const VCD_KEYWORDS: [&str;12] =
// 			["$commet","$date","$end","$timescale","$var",
// 			"$upscope","$enddefinitions","$dumpvars","$version","$dumpall",
// 			"$scope","dumpoff","dumpon"];
use std::cell::RefCell;
use crate::parser::*;
use crate::error::BadVCDReport;
use serde::{Serialize, Deserialize};

mod error;
mod parser;
mod saver;

use parser::{VcdTag,vcd_parser};

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


#[derive(Debug)]
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
    // pub padding_value: Vec<Vec<usize>>,
}

impl VcdDb {
    pub fn new() -> Self {
        VcdDb::default()
    }

    pub fn aligned(&self) -> bool {
        let dumped_var_num = self.var_value[0].len();
        for val in &self.var_value {
            if val.len() != dumped_var_num {
                return false;
            }
        }
        return true;
    }

    // result: 
    // 1. clean data in padding value and align data in var_value
    // 2. clean data in value_var_map


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


pub fn parse_vcd(file: &str) -> Result<VcdDb, VcdError> {
    // let report = VcdIoReport::new(&file);
    // let buff = std::fs::read_to_string(&file)?;
    let vcd: VcdDb = vcd_parser(file)?;
    Ok(vcd)
}

#[derive(Default,Debug)]
pub struct LineBuffer {
    data:Vec<u8>,
}

use std::sync::mpsc::channel;


pub fn mt_parse_vcd(reader:std::fs::File) -> std::result::Result<VcdDb, VcdError> {
    let (sender, receiver) = channel::<LineBuffer>();

    let hd = std::thread::spawn(move || {
        let mut reader = ReaderWrap {reader,line_num:0u64};
        if !reader.read(sender).is_ok() {
            // TODO: not return error when enter this
            return Err(VcdError::InternalError);
        }
        Ok(())
    });
    let vcd = parse_by_line_buffer(receiver);

    if !hd.join().is_ok() {
        return Err(VcdError::InternalError);
    }

    vcd
}


use std::sync::mpsc::{Receiver,Sender};


pub struct ReaderWrap<R:std::io::Read> {
    reader:R,
    line_num:u64,
}

const COMMAND_END: &[u8] = b"$end";
const COMMAND_END_LEN: usize = b"$end".len();


fn check_contain_last_end(input:&[u8]) -> bool {
    input[(input.len() - COMMAND_END_LEN)..] == *COMMAND_END
}

impl<R: std::io::Read> ReaderWrap<R> {
    fn read_byte_or_eof(&mut self) -> Result<Option<u8>, VcdError> {
        let b = std::io::Read::bytes(&mut self.reader).next().transpose();

        let end_of_line = matches!(b, Ok(Some(b'\n')));

        // delay incrementing the line number until the first character of the next line
        // so that errors at the end of the line refer to the correct line number
        if end_of_line {
            self.line_num += 1;
        }

        b.map_err(|_|VcdError::BadVCD(BadVCDReport{error_start_line:self.line_num,..Default::default()}))
    }

    fn read_byte(&mut self) -> Result<u8, VcdError> {
        match self.read_byte_or_eof()? {
            None => Err(VcdError::BadVCD(BadVCDReport{error_start_line:self.line_num,..Default::default()})),
            Some(v) => Ok(v),
        }
    }




    fn read(&mut self,sender:Sender<LineBuffer>) -> Result<(), VcdError>  {
        let mut line_data = vec![];
        loop {
            let b = self.read_byte()?;
            if !matches!(b, b'\n') {
                line_data.push(b);
            } else {
                let line_buffer = LineBuffer {data:line_data.clone()};
                println!("send data {:?}", line_buffer);
                if check_contain_last_end(&line_buffer.data) {
                    if !sender.send(line_buffer).is_ok() {
                        println!("enter read error");
                        return Err(VcdError::InternalError);
                    }
                    line_data.clear();    
                } else {
                    // padding a space after /n
                    line_data.push(b'\n');
                }

            }
        }
    }
}

    fn parse_by_line_buffer(rcver:Receiver<LineBuffer>) -> std::result::Result<VcdDb, VcdError> {
        
        let mut vcd_db = VcdDb::new();

        let scope_index_chain: RefCell<Vec<ScopeIndex>> = RefCell::new(vec![]);
        // let mut vcd_statement_slices = vec![];
        let mut scope_idx: usize = 0;
        let mut var_idx: usize = 0;

        let _has_timestamped_value = false;
        let _has_dumpvars = RefCell::new(false);
        let mut statement_start_without_end = false;
        let mut readed_line: u64 = 0;
        let mut onprocess_tag = VcdTag::Untagged;
        let mut buff = vec![];
        let mut line_num = 0u64;

        let mut timestamp_rcvd = false;
        let mut prev_var_value = vec![];
        // let mut prev_var_value_mask = vec![];
        let mut dumpvars_index_map:HashMap<usize,usize> = HashMap::default();


        let mut count = 0;
        while let Ok(line_buff) = rcver.recv() {
            println!("enter mt line buffer parser, the counter is {}",count);
            let line = String::from_utf8(line_buff.data).unwrap();
            line_num += 1;
            println!("current line is {:?}",line);
            let rcvd_leading_tag = peak_tag(&line);
            if let Some(tag) = rcvd_leading_tag {
                onprocess_tag = tag;
            }
            
            let exception_report = match rcvd_leading_tag {
                Some(leading_tag) => {
                    println!("current tag is {:?}", leading_tag);
                    if leading_tag == VcdTag::Unsupported {
                        Some(BadVCDReport {
                            recovered_buff: buff.join(" "),
                            error_start_line: line_num,
                            possible_error: format!(
                                "Unsupported tag is found at line {}",
                                line_num
                            ),
                        })
                    } else if leading_tag == VcdTag::Untagged
                        && !timestamp_rcvd
                        && !statement_start_without_end
                    {
                        Some(BadVCDReport {
                            recovered_buff: buff.join(" "),
                            error_start_line: line_num,
                            possible_error: "Untagged is not allowed before timestamp or after statement has end".to_string(),
                        })
                    } else {
                        if leading_tag == VcdTag::Timestamp {
                            timestamp_rcvd = true;
                            if buff.is_empty() {
                                buff.push(line);
                            } else {
                                // first parse variable, then clear, finally push
                                // 1.
                                let casted = buff.join("\n");
                                let (_, res) = vcd_variable_parser(&casted).map_err(|_| {
                                    let mut diagnosis = BadVCDReport::new();
                                    diagnosis.recovered_buff = casted.clone();
                                    diagnosis.error_start_line = line_num;
                                    diagnosis.possible_error = format!("found invalid variable var at line {}",line_num);                              
                                    VcdError::BadVCD(diagnosis)
                                })?;
                                vcd_db.timestap.push(res.0);
                                if vcd_db.var_value.is_empty() {
                                    let mut zero_stamp_values: Vec<VarValue> = vec![];
                                    let mut zero_stamp_value_cnt = 0usize;
                                    // let mut padding_vec = vec![];
                                    for (item1,item2) in res.1.iter().enumerate() {
                                        let var_id = *vcd_db.var_id_map.get(item2.1).unwrap_or_else(||panic!("current id is {:?}, current var id map {:?}", item2.1,vcd_db.var_id_map));
                                        let target_width = vcd_db.variable[var_id].width as usize;
                                        let mut un_padding = (item2.0).clone();
                                        if !un_padding.padding(target_width) {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = casted.clone();
                                            diagnosis.error_start_line = line_num;
                                            diagnosis.possible_error = format!("found invalid variable var at line {}",line_num);  
                                            return Err(VcdError::BadVCD(diagnosis));
                                        }
                                        zero_stamp_values.push(un_padding);
                                        // prev_var_value_mask.push(false); // all is not masked
                                        dumpvars_index_map.insert(var_id,zero_stamp_value_cnt);
                                        vcd_db.value_var_map.insert(item1, (item2.1).to_string());
                                        zero_stamp_value_cnt += 1;
                                        // padding_vec.push(var_id); // TODO add sever fatal                                                                                
                                    }
                                    // vcd_db.padding_value.push(padding_vec);
                                    prev_var_value = zero_stamp_values.clone();
                                    vcd_db.var_value.push(zero_stamp_values);
                                } else {
                                    // let mut padding_vec = vec![];
                                    let mut value_vec = prev_var_value.clone();
                                    for item in res.1 {
                                        let var_id = *vcd_db.var_id_map.get(item.1).unwrap_or_else(||panic!("current id is {:?}, current var id map {:?}", item.1,vcd_db.var_id_map));
                                        let target_width = vcd_db.variable[var_id].width as usize;
                                        let mut un_padding = (item.0).clone();
                                        if !un_padding.padding(target_width) {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = casted.clone();
                                            diagnosis.error_start_line = line_num;
                                            diagnosis.possible_error = format!("found invalid variable var at line {}",line_num);  
                                            return Err(VcdError::BadVCD(diagnosis));
                                        }                                 
                                        // padding_vec.push(var_id); // TODO add sever fatal
                                        // value_vec.push(un_padding);
                                        if let Some(idx) = dumpvars_index_map.get(&var_id) {
                                            value_vec[*idx] = un_padding;
                                        } else {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = casted.clone();
                                            diagnosis.error_start_line = line_num;
                                            diagnosis.possible_error = format!("found invalid variable var at line {}",line_num);  
                                            return Err(VcdError::BadVCD(diagnosis));
                                        }
                                    }

                                    prev_var_value = value_vec.clone();
                                    vcd_db
                                        .var_value
                                        .push(value_vec);
                                    // vcd_db.padding_value.push(padding_vec);
                                }
                                // 2.
                                buff.clear();
                                // 3.
                                buff.push(line);
                            }
                        } else {
                            // parse dump ctrl and not append buffer
                            if timestamp_rcvd && check_skipped_after_timestamp(&leading_tag) {
                                // TODO: dump ctrl should be skipped when it come along inside timestamp
                                // TODO: comment should be skipped when it come along inside timestamp
                            } else {
                                buff.push(line);
                            }
                            // let casted = buff.join("\n");
                            // println!("casted is {:?}", casted);
                            if check_end(&buff) {
                                // 1.reset state
                                statement_start_without_end = false;
                                // 2.get casted
                                let casted = buff.join("\n");                           
                                // 3.clear buff
                                buff.clear();
                                match onprocess_tag {
                                    VcdTag::Version => {
                                        let (_, version) =
                                            vcd_version_parser(&casted).map_err(|_| {
                                                let mut diagnosis = BadVCDReport::new();
                                                diagnosis.recovered_buff = casted.clone();
                                                diagnosis.error_start_line = readed_line;
                                                VcdError::BadVCD(diagnosis)
                                            })?;
                                        vcd_db.version = version.to_string();
                                    }
                                    VcdTag::Comment => {
                                        let (_, comment) =
                                            vcd_comment_parser(&casted).map_err(|_| {
                                                let mut diagnosis = BadVCDReport::new();
                                                diagnosis.recovered_buff = casted.clone();
                                                diagnosis.error_start_line = readed_line;
                                                VcdError::BadVCD(diagnosis)
                                            })?;
                                        vcd_db.comment = comment.to_string();
                                    }
                                    VcdTag::Date => {
                                        let (_, date) = vcd_date_parser(&casted).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = casted.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                        vcd_db.date = date.to_string();
                                    }
                                    VcdTag::Timescale => {
                                        let (_, timescale) = vcd_timescale_parser(&casted)
                                            .map_err(|_| {
                                                let mut diagnosis = BadVCDReport::new();
                                                diagnosis.recovered_buff = casted.clone();
                                                diagnosis.error_start_line = readed_line;
                                                VcdError::BadVCD(diagnosis)
                                            })?;
                                        vcd_db.timescale = timescale;
                                    }
                                    VcdTag::VarDef => {
                                        let (_, res) =
                                            vcd_variable_def_parser(&casted).map_err(|_| {
                                                let mut diagnosis = BadVCDReport::new();
                                                diagnosis.recovered_buff = casted.clone();
                                                diagnosis.error_start_line = readed_line;
                                                diagnosis.possible_error = format!("found bad variable def at line {}",readed_line);                                                
                                                VcdError::BadVCD(diagnosis)
                                            })?;
                                        vcd_db.variable.push(res.1);
                                        vcd_db.var_id_map.insert(res.0, var_idx);
                                        vcd_db.scope[*scope_index_chain.borrow().last().unwrap()]
                                            .variables
                                            .push(var_idx);
                                        var_idx += 1;
                                    }
                                    VcdTag::Scope => {
                                        let (_, res) = vcd_scope_def_parser(&casted).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = casted.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                        let a_scope: Scope = Scope {
                                            scope_type: res.0,
                                            scope_name: res.1.to_string(),
                                            sub_scope_idx: vec![],
                                            variables: vec![],
                                        };
                                        vcd_db.scope.push(a_scope);

                                        scope_index_chain.borrow_mut().push(scope_idx);
                                        scope_idx += 1;
                                    }
                                    VcdTag::Unscope => {
                                        let _ = vcd_upscope_parser(&casted).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = casted.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                        let _ = scope_index_chain.borrow_mut().pop();
                                    }
                                    VcdTag::Dumpvars | VcdTag::Dumpports | VcdTag::DumpOn | VcdTag::DumpOff | VcdTag::DumpAll => {

                                    }
                                    _ => {
                                        // todo:
                                    }
                                }
                            } else {
                                // set state
                                statement_start_without_end = true;
                            }
                        }

                        None
                    }
                }
                None => {
                    readed_line += 1;
                    None
                }
                // Some(BadVCDReport {
                //     recovered_buff: buff.join(" "),
                //     error_start_line: readed_line,
                //     possible_error: format!("parser fail, not known error"),
                // }),
            };
            if let Some(rpt) = exception_report {
                return Err(VcdError::BadVCD(rpt));
            };

            count += 1;
        }

        println!("exit normally");

        Ok(vcd_db)
    }




