use std::cell::RefCell;
use std::str;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_until1},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, one_of, space1},
    combinator::{map, map_res, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, tuple},
};

use crate::error::BadVCDReport;
use crate::error::VcdError;
use crate::*;

const SPECIAL_IDENTIFIER: &str = "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~0123456789ABCDEFGHIJKLMNOPGRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

pub fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> VcdRes<&'a str, O>
where
    F: FnMut(&'a str) -> VcdRes<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

pub fn tstring(input: &str) -> VcdRes<&str, &str> {
    ws(recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    )))(input)
}

pub(crate) fn vcd_date_parser(input: &str) -> VcdRes<&str, &str> {
    map(
        delimited(ws(tag("$date")), take_until1("$end"), tag("$end")),
        str::trim,
    )(input)
}

pub(crate) fn vcd_version_parser(input: &str) -> VcdRes<&str, &str> {
    map(
        delimited(ws(tag("$version")), take_until1("$end"), tag("$end")),
        str::trim,
    )(input)
}

pub(crate) fn vcd_comment_parser(input: &str) -> VcdRes<&str, &str> {
    map(
        delimited(ws(tag("$comment")), take_until1("$end"), tag("$end")),
        str::trim,
    )(input)
}

pub(crate) fn vcd_timescale_parser(input: &str) -> VcdRes<&str, (u32, TimeUnit)> {
    delimited(
        ws(tag("$timescale")),
        ws(tuple((
            ws(map(digit1, |s: &str| s.parse().unwrap())),
            alt((
                map(tag("ps"), |_| TimeUnit::Psec),
                map(char('s'), |_| TimeUnit::Sec),
                map(tag("ms"), |_| TimeUnit::Msec),
                map(tag("us"), |_| TimeUnit::Usec),
                map(tag("ns"), |_| TimeUnit::Nsec),
            )),
        ))),
        ws(tag("$end")),
    )(input)
}

pub(crate) fn vcd_width_parser(input: &str) -> VcdRes<&str, u16> {
    alt((
        map(digit1, |d: &str| d.parse().unwrap()),
        map(
            tuple((char('['), digit1, char(':'), digit1, char(']'))),
            |d: (char, &str, char, &str, char)| {
                d.1.parse::<u16>().unwrap() - d.3.parse::<u16>().unwrap()
            },
        ),
    ))(input)
}

pub(crate) fn vcd_identifier_parser(input: &str) -> VcdRes<&str, String> {
    map(many1(one_of(SPECIAL_IDENTIFIER)), |res| {
        res.iter().collect()
    })(input)
}

pub(crate) fn vcd_variable_def_parser(input: &str) -> VcdRes<&str, (String, Variable)> {
    map(
        delimited(
            ws(tag("$var")),
            tuple((
                alt((
                    map(tag("wire"), |_| VarType::Wire),
                    map(tag("reg"), |_| VarType::Reg),
                    map(tag("trireg"), |_| VarType::Reg),
                    map(tag("integer"), |_| VarType::Integer),
                    map(tag("port"), |_| VarType::Port),
                )),
                space1,
                vcd_width_parser,
                space1,
                vcd_identifier_parser,
                space1,
                tstring,
                take_until("$"),
            )),
            ws(tag("$end")),
        ),
        |res: (VarType, &str, u16, &str, _, &str, &str, &str)| {
            (
                res.4,
                Variable {
                    var_type: res.0,
                    name: res.6.to_string(),
                    width: res.2,
                },
            )
        },
    )(input)
}

pub(crate) fn vcd_scope_def_parser(input: &str) -> VcdRes<&str, (ScopeType, &str)> {
    delimited(
        ws(tag("$scope")),
        tuple((
            alt((
                map(tag("module"), |_| ScopeType::Module),
                map(tag("task"), |_| ScopeType::Task),
                map(tag("function"), |_| ScopeType::Function),
                map(tag("fork"), |_| ScopeType::Fork),
            )),
            tstring,
        )),
        ws(tag("$end")),
    )(input)
}

pub(crate) fn vcd_upscope_parser(input: &str) -> VcdRes<&str, ()> {
    value((), pair(ws(tag("$upscope")), tag("$end")))(input)
}

pub(crate) fn vcd_enddefinition_parser(input: &str) -> VcdRes<&str, ()> {
    value((), pair(ws(tag("$enddefinitions")), tag("$end")))(input)
}

pub(crate) fn vcd_scalar_val_parser(input: &str) -> VcdRes<&str, ScalarValue> {
    ws(alt((
        map(recognize(char('0')), |_| ScalarValue::ZeroOne(false)),
        map(recognize(char('1')), |_| ScalarValue::ZeroOne(true)),
        map(recognize(char('x')), |_| ScalarValue::Xstate),
        map(recognize(char('X')), |_| ScalarValue::Xstate),
    )))(input)
}

pub(crate) fn vcd_vector_val_parser(input: &str) -> VcdRes<&str, Vec<ScalarValue>> {
    preceded(
        ws(alt((char('b'), char('B')))),
        many1(alt((
            map(char('0'), |_| ScalarValue::ZeroOne(false)),
            map(char('1'), |_| ScalarValue::ZeroOne(true)),
            map(char('x'), |_| ScalarValue::Xstate),
            map(char('X'), |_| ScalarValue::Xstate),
        ))),
    )(input)
}

pub(crate) fn vcd_real_val_parser(input: &str) -> VcdRes<&str, String> {
    map(
        preceded(ws(alt((char('R'), char('r')))), digit1),
        |res: &str| res.to_string(),
    )(input)
}

pub(crate) fn vcd_variable_val_parser(input: &str) -> VcdRes<&str, (VarValue, &str)> {
    pair(
        alt((
            map(vcd_scalar_val_parser, |v| VarValue::Scalar(v)),
            map(vcd_vector_val_parser, |v| VarValue::Vector(v)),
            map(vcd_real_val_parser, |v| VarValue::Real(v)),
        )),
        take_until1("\n"),
    )(input)
}

pub(crate) fn vcd_timestap_parser(input: &str) -> VcdRes<&str, u32> {
    preceded(ws(char('#')), map_res(digit1, str::parse))(input)
}

pub(crate) fn vcd_variable_parser(input: &str) -> VcdRes<&str, (u32, Vec<(VarValue, &str)>)> {
    tuple((vcd_timestap_parser, many1(vcd_variable_val_parser)))(input)
}

pub(crate) fn vcd_dumpvars_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
    delimited(
        ws(tag("$dumpvars")),
        many1(vcd_variable_val_parser),
        ws(tag("$end")),
    )(input)
}

pub(crate) fn vcd_dumpall_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
    delimited(
        ws(tag("$dumpall")),
        many1(vcd_variable_val_parser),
        ws(tag("$end")),
    )(input)
}

pub(crate) fn vcd_dumpoff_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
    delimited(
        ws(tag("$dumpoff")),
        many1(vcd_variable_val_parser),
        ws(tag("$end")),
    )(input)
}

pub(crate) fn vcd_dumpon_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
    delimited(
        ws(tag("$dumpon")),
        many1(vcd_variable_val_parser),
        ws(tag("$end")),
    )(input)
}

#[derive(PartialEq)]
pub enum VcdTag {
    Timestamp, // timestamp start with #
    Date,
    Timescale,
    Version,
    Comment,
    VarDef,
    Scope,
    Unscope,
    Dumpvars,
    DumpOff,
    DumpOn,
    DumpAll,
    End,
    EndDef,
    Untagged,
    Unsupported,
}

impl Default for VcdTag {
    fn default() -> Self {
        VcdTag::End
    }
}

const SPLIT_PATTERN: [char; 2] = [' ', '\n'];

fn peak_tag(input: &str) -> Option<VcdTag> {
    let trimed = input.trim_start();
    if trimed.starts_with("#") {
        Some(VcdTag::Timestamp)
    } else if trimed.starts_with("$") {
        if let Some((leading_tag, _)) = trimed.split_once(&SPLIT_PATTERN) {
            let matched = match leading_tag {
                "$end" => VcdTag::End,
                "$date" => VcdTag::Date,
                "$timescale" => VcdTag::Timescale,
                "$version" => VcdTag::Version,
                "$comment" => VcdTag::Comment,
                "$var" => VcdTag::VarDef,
                "$scope" => VcdTag::Scope,
                "$upscope" => VcdTag::Unscope,
                "$dumpvars" => VcdTag::Dumpvars,
                "$enddefinitions" => VcdTag::EndDef,
                "$dumpoff" => VcdTag::DumpOff,
                "$dumpon" => VcdTag::DumpOn,
                "$dumpall" => VcdTag::DumpAll,
                _ => VcdTag::Unsupported,
            };
            Some(matched)
        } else {
            None
        }
    } else {
        Some(VcdTag::Untagged)
    }
}

fn check_end(input: &str) -> bool {
    if input.trim_end().ends_with("$end") {
        true
    } else {
        false
    }
}

fn check_end_contained(input: &str) -> bool {
    if input.contains("$end") {
        true
    } else {
        false
    }
}

pub(crate) fn vcd_parser(input: &str) -> std::result::Result<VcdDb, VcdError> {
    use std::io::{self, BufRead};

    let mut cursor = io::Cursor::new(input);
    let mut line_buf = String::new();
    let mut vcd_db = VcdDb::new();

    let scope_index_chain: RefCell<Vec<ScopeIndex>> = RefCell::new(vec![]);
    let mut scope_idx: usize = 0;
    let mut var_idx: usize = 0;
    let has_dumpvars = RefCell::new(false);
    let mut readed_line: u64 = 0;

    loop {
        match cursor.read_line(&mut line_buf) {
            Ok(n) => {
                if n == 0usize {
                    // eof reached
                    return Ok(vcd_db);
                } else {
                    if !check_end(&line_buf) && !check_end_contained(&line_buf) {
                        // println!("continue");
                    } else {
                        if let Some(tag) = peak_tag(&line_buf) {
                            match tag {
                                VcdTag::Version => {
                                    let (_, version) =
                                        vcd_version_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                    vcd_db.version = version.to_string();
                                }
                                VcdTag::Comment => {
                                    let (_, comment) =
                                        vcd_comment_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                    vcd_db.comment = comment.to_string();
                                }
                                VcdTag::Date => {
                                    let (_, date) = vcd_date_parser(&line_buf).map_err(|_| {
                                        let mut diagnosis = BadVCDReport::new();
                                        diagnosis.recovered_buff = line_buf.clone();
                                        diagnosis.error_start_line = readed_line;
                                        VcdError::BadVCD(diagnosis)
                                    })?;
                                    vcd_db.date = date.to_string();
                                }
                                VcdTag::Timescale => {
                                    let (_, timescale) =
                                        vcd_timescale_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                    vcd_db.timescale = timescale;
                                }
                                VcdTag::Scope => {
                                    let (_, res) =
                                        vcd_scope_def_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
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
                                    let _ = vcd_upscope_parser(&line_buf).map_err(|_| {
                                        let mut diagnosis = BadVCDReport::new();
                                        diagnosis.recovered_buff = line_buf.clone();
                                        diagnosis.error_start_line = readed_line;
                                        VcdError::BadVCD(diagnosis)
                                    })?;
                                    let _ = scope_index_chain.borrow_mut().pop();
                                }
                                VcdTag::Dumpvars => {
                                    let (_, res) =
                                        vcd_dumpvars_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                    // dump var should be recorded

                                    *has_dumpvars.borrow_mut() = true;
                                    for (i, (_, s)) in res.iter().enumerate() {
                                        vcd_db.value_var_map.insert(i, s.to_string());
                                    }
                                }
                                VcdTag::DumpOff => {
                                    let (_, _res) =
                                        vcd_dumpoff_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                }
                                VcdTag::DumpOn => {
                                    let (_, res) = vcd_dumpon_parser(&line_buf).map_err(|_| {
                                        let mut diagnosis = BadVCDReport::new();
                                        diagnosis.recovered_buff = line_buf.clone();
                                        diagnosis.error_start_line = readed_line;
                                        VcdError::BadVCD(diagnosis)
                                    })?;
                                    if vcd_db.var_value.len() == 0 {
                                        let mut zero_stamp_values: Vec<VarValue> = vec![];
                                        for (j, (a_v, s)) in res.iter().enumerate() {
                                            zero_stamp_values.push(a_v.clone());
                                            vcd_db.value_var_map.insert(j, s.to_string());
                                        }
                                        vcd_db.var_value.push(zero_stamp_values);
                                    } else {
                                        vcd_db
                                            .var_value
                                            .push(res.iter().map(|x| x.0.clone()).collect());
                                    }
                                }
                                VcdTag::DumpAll => {
                                    let (_, res) = vcd_dumpall_parser(&line_buf).map_err(|_| {
                                        let mut diagnosis = BadVCDReport::new();
                                        diagnosis.recovered_buff = line_buf.clone();
                                        diagnosis.error_start_line = readed_line;
                                        VcdError::BadVCD(diagnosis)
                                    })?;
                                    if vcd_db.var_value.len() == 0 {
                                        let mut zero_stamp_values: Vec<VarValue> = vec![];
                                        for (j, (a_v, s)) in res.iter().enumerate() {
                                            zero_stamp_values.push(a_v.clone());
                                            vcd_db.value_var_map.insert(j, s.to_string());
                                        }
                                        vcd_db.var_value.push(zero_stamp_values);
                                    } else {
                                        vcd_db
                                            .var_value
                                            .push(res.iter().map(|x| x.0.clone()).collect());
                                    }
                                }
                                VcdTag::VarDef => {
                                    let (_, res) =
                                        vcd_variable_def_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                    vcd_db.variable.push(res.1);
                                    vcd_db.var_id_map.insert(res.0, var_idx);
                                    vcd_db.scope[*scope_index_chain.borrow().last().unwrap()]
                                        .variables
                                        .push(var_idx);
                                    var_idx += 1;
                                }
                                VcdTag::Timestamp => {
                                    let (_, res) =
                                        vcd_timestap_parser(&line_buf).map_err(|_| {
                                            let mut diagnosis = BadVCDReport::new();
                                            diagnosis.recovered_buff = line_buf.clone();
                                            diagnosis.error_start_line = readed_line;
                                            VcdError::BadVCD(diagnosis)
                                        })?;
                                    vcd_db.timestap.push(res);
                                }
                                VcdTag::End => {}
                                VcdTag::EndDef => {}
                                _ => {
                                    let mut report = BadVCDReport::default();
                                    report.recovered_buff = line_buf.clone();
                                    report.error_start_line = readed_line;
                                    unreachable!("report {:?}", report)
                                }
                            }
                            // TODO not flush line buff if $end is not at line end
                            line_buf.clear();
                        } else {
                            let mut report = BadVCDReport::default();
                            report.recovered_buff = line_buf.clone();
                            report.error_start_line = readed_line;
                            unreachable!("report {:?}", report)
                        }
                    }
                }
            }
            Err(_) => panic!("readline error"),
        }
        readed_line += 1;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_anychar() {
        assert_eq!(
            vcd_identifier_parser("abcd_efg"),
            Ok(("", "abcd_efg".to_string()))
        );
        assert_eq!(
            vcd_identifier_parser("abcd efg"),
            Ok((" efg", "abcd".to_string()))
        );
        assert_eq!(
            vcd_identifier_parser("#* abcd"),
            Ok((" abcd", "#*".to_string()))
        );
    }
    #[test]
    fn test_vcd_variable_val_parser() {
        assert_eq!(
            vcd_variable_val_parser("b1001 #\n"),
            Ok((
                "\n",
                (
                    VarValue::Vector(vec![
                        ScalarValue::ZeroOne(true),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(true)
                    ]),
                    " #"
                )
            ))
        );
        assert_eq!(
            vcd_variable_val_parser("x&\n"),
            Ok(("\n", (VarValue::Scalar(ScalarValue::Xstate), "&")))
        );
    }

    #[test]
    fn test_vcd_parser() {
        let vcd_plain = concat!(
            "$date Date text. For example: November 11, 2009. $end\n",
            "$version VCD generator tool version info text. $end\n",
            "$comment Any comment text. $end\n",
            "$timescale 1ps $end\n",
            "$scope module logic $end\n",
            "$var wire 8 # data $end\n",
            "$var wire 1 $ data_valid $end\n",
            "$var wire 1 % en $end\n",
            "$var wire 1 & rx_en $end\n",
            "$var wire 1 ' tx_en $end\n",
            "$var wire 1 ( empty $end\n",
            "$var wire 1 ) underrun $end\n",
            "$upscope $end\n",
            "$enddefinitions $end\n",
            "$dumpvars\n",
            "bxxxxxxxx #\n",
            "x$\n",
            "0%\n",
            "x&\n",
            "x'\n",
            "1(\n",
            "0)\n",
            "$end",
            "#0\n",
            "b10000001 #\n",
            "0$\n",
            "1%\n",
            "0&\n",
            "1'\n",
            "0(\n",
            "0)\n",
            "#2211\n",
            "0'\n",
            "#2296\n",
            "b0 #\n",
            "1$\n",
            "#2302\n",
            "0$\n",
            "#2303\n",
        );
        let vcd_db: VcdDb = vcd_parser(&vcd_plain).unwrap();
        assert_eq!(
            vcd_db.date,
            "Date text. For example: November 11, 2009.".to_string()
        );
        assert_eq!(vcd_db.comment, "Any comment text.".to_string());
        assert_eq!(
            vcd_db.version,
            "VCD generator tool version info text.".to_string()
        );
        assert_eq!(vcd_db.timescale.1, TimeUnit::Psec);
        assert_eq!(
            vcd_db.variable[0],
            Variable {
                var_type: VarType::Wire,
                name: "data".to_string(),
                width: 8
            }
        );
        assert_eq!(vcd_db.var_id_map.get("#"), Some(&0));
    }
}
