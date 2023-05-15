use std::cell::RefCell;
use std::fs::File;
use std::io::BufRead;
use std::path::Path;
use std::str;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_until1},
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
    alt((
        map(recognize(char('0')), |_| ScalarValue::ZeroOne(false)),
        map(recognize(char('1')), |_| ScalarValue::ZeroOne(true)),
        map(recognize(char('x')), |_| ScalarValue::Xstate),
        map(recognize(char('X')), |_| ScalarValue::Xstate),
        map(recognize(char('z')), |_| ScalarValue::Zstate),
        map(recognize(char('Z')), |_| ScalarValue::Zstate),
    ))(input)
}

pub(crate) fn vcd_vector_val_parser(input: &str) -> VcdRes<&str, Vec<ScalarValue>> {
    preceded(alt((char('b'), char('B'))), many1(vcd_scalar_val_parser))(input)
}

pub(crate) fn vcd_real_val_parser(input: &str) -> VcdRes<&str, String> {
    map(
        preceded(alt((char('R'), char('r'))), digit1),
        |res: &str| res.to_string(),
    )(input)
}

pub(crate) fn vcd_variable_val_parser(input: &str) -> VcdRes<&str, (VarValue, &str)> {
    alt((
        pair(
            ws(map(vcd_scalar_val_parser, |v| VarValue::Scalar(v))),
            take_till(|c| c == ' ' || c == '\n'),
        ),
        pair(
            ws(map(vcd_vector_val_parser, |v| VarValue::Vector(v))),
            map(take_till(|c| c == '\n'), str::trim),
        ),
        pair(
            ws(map(vcd_real_val_parser, |v| VarValue::Real(v))),
            take_till(|c| c == ' ' || c == '\n'),
        ),
    ))(input)
}

pub(crate) fn vcd_timestap_parser(input: &str) -> VcdRes<&str, u32> {
    ws(preceded(char('#'), map_res(digit1, str::parse)))(input)
}

pub(crate) fn vcd_variable_parser(input: &str) -> VcdRes<&str, (u32, Vec<(VarValue, &str)>)> {
    tuple((vcd_timestap_parser, many1(vcd_variable_val_parser)))(input)
}

pub(crate) fn vcd_dumpctrl_parser(input: &str) -> VcdRes<&str, VcdDumpCtrl> {
    alt((
        map(ws(tag("$dumpvars")), |_| VcdDumpCtrl::Dumpvars),
        map(ws(tag("$dumpports")), |_| VcdDumpCtrl::Dumpports),
        map(ws(tag("$dumpall")), |_| VcdDumpCtrl::DumpAll),
        map(ws(tag("$dumpon")), |_| VcdDumpCtrl::DumpOn),
        map(ws(tag("$dumpoff")), |_| VcdDumpCtrl::DumpOff),
    ))(input)
}

// pub(crate) fn vcd_dumpvars_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
//     delimited(
//         ws(tag("$dumpvars")),
//         many1(vcd_variable_val_parser),
//         ws(tag("$end")),
//     )(input)
// }

// pub(crate) fn vcd_dumpports_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
//     delimited(
//         ws(tag("$dumpports")),
//         many1(vcd_variable_val_parser),
//         ws(tag("$end")),
//     )(input)
// }

// pub(crate) fn vcd_dumpall_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
//     delimited(
//         ws(tag("$dumpall")),
//         many1(vcd_variable_val_parser),
//         ws(tag("$end")),
//     )(input)
// }

// pub(crate) fn vcd_dumpoff_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
//     delimited(
//         ws(tag("$dumpoff")),
//         many1(vcd_variable_val_parser),
//         ws(tag("$end")),
//     )(input)
// }

// pub(crate) fn vcd_dumpon_parser(input: &str) -> VcdRes<&str, Vec<(VarValue, &str)>> {
//     delimited(
//         ws(tag("$dumpon")),
//         many1(vcd_variable_val_parser),
//         ws(tag("$end")),
//     )(input)
// }

#[derive(PartialEq, Debug)]
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
    Dumpports,
    DumpOff,
    DumpOn,
    DumpAll,
    End,
    EndDef,
    Untagged,
    Unsupported,
}

#[derive(PartialEq, Debug)]
pub enum VcdDumpCtrl {
    Dumpvars,
    Dumpports,
    DumpOff,
    DumpOn,
    DumpAll,
}

impl Default for VcdTag {
    fn default() -> Self {
        VcdTag::End
    }
}

const SPLIT_PATTERN: [char; 1] = [' '];

fn peak_tag(input: &str) -> Option<VcdTag> {
    let trimed = input.trim();
    if trimed.starts_with("#") {
        Some(VcdTag::Timestamp)
    } else if trimed.starts_with("$") {
        if let Some(leading_tag) = trimed.split_whitespace().nth(0) {
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
                "$dumpports" => VcdTag::Dumpports,
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

fn check_timestamp(input: &str) -> bool {
    if input.starts_with('#') {
        true
    } else {
        false
    }
}

fn check_skipped_after_timestamp(leading_tag: &VcdTag) -> bool {
    if *leading_tag == VcdTag::Comment
        || *leading_tag == VcdTag::Dumpports
        || *leading_tag == VcdTag::Dumpvars
        || *leading_tag == VcdTag::DumpAll
        || *leading_tag == VcdTag::DumpOff
        || *leading_tag == VcdTag::DumpOn
        || *leading_tag == VcdTag::End
    // dump end
    {
        true
    } else {
        false
    }
}

use std::io;
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

pub(crate) fn vcd_parser(input: &str) -> std::result::Result<VcdDb, VcdError> {
    // let mut cursor = io::Cursor::new(input);
    let mut vcd_db = VcdDb::new();

    let scope_index_chain: RefCell<Vec<ScopeIndex>> = RefCell::new(vec![]);
    // let mut vcd_statement_slices = vec![];
    let mut scope_idx: usize = 0;
    let mut var_idx: usize = 0;

    let _has_timestamped_value = false;
    let _has_dumpvars = RefCell::new(false);
    let mut statement_start_without_end = false;
    let mut readed_line: u64 = 0;
    let mut buff = vec![];

    if let Ok(lines) = read_lines(input) {
        let mut timestamp_rcvd = false;
        for line in lines {
            let rcvd_leading_tag = if let Ok(ref slice) = line {
                if !slice.is_empty() {
                    peak_tag(&slice)
                } else {
                    None
                }
            } else {
                None
            };
            let exception_report = match rcvd_leading_tag {
                Some(leading_tag) => {
                    if leading_tag == VcdTag::Unsupported {
                        Some(BadVCDReport {
                            recovered_buff: buff.join(" "),
                            error_start_line: readed_line,
                            possible_error: format!(
                                "Unsupported tag is found at line {}",
                                readed_line
                            ),
                        })
                    } else if leading_tag == VcdTag::Untagged
                        && !timestamp_rcvd
                        && !statement_start_without_end
                    {
                        Some(BadVCDReport {
                            recovered_buff: buff.join(" "),
                            error_start_line: readed_line,
                            possible_error: format!("Untagged is not allowed before timestamp or after statement has end"),
                        })
                    } else {
                        // normal flow, not expect exception here
                        readed_line += 1;
                        if leading_tag == VcdTag::Untagged
                        && (timestamp_rcvd || statement_start_without_end) {
                        	buff.push(line.unwrap());
                        } else if leading_tag == VcdTag::Timestamp {
                            timestamp_rcvd = true;
                            if buff.len() == 0usize {
                                buff.push(line.unwrap());
                            } else {
                                // first parse variable, then clear, finally push
                                // 1.
                                let casted = buff.join("\n");
                                let (_, res) = vcd_variable_parser(&casted).map_err(|_| {
                                    let mut diagnosis = BadVCDReport::new();
                                    diagnosis.recovered_buff = casted.clone();
                                    diagnosis.error_start_line = readed_line;
                            		diagnosis.possible_error = format!("found invalid variable var at line {}",readed_line);                              
                                    VcdError::BadVCD(diagnosis)
                                })?;
                                vcd_db.timestap.push(res.0);
                                if vcd_db.var_value.len() == 0 {
                                    let mut zero_stamp_values: Vec<VarValue> = vec![];
                                    let mut padding_vec = vec![];
                                    res.1.iter().enumerate().for_each(|(item1,item2)|{
                                        zero_stamp_values.push((item2.0).clone());
                                        vcd_db.value_var_map.insert(item1, (item2.1).to_string());
                                        padding_vec.push(*vcd_db.var_id_map.get(item2.1).expect(&format!("current id is {:?}, current var id map {:?}", item2.1,vcd_db.var_id_map))); // TODO add sever fatal                                                                                
                                    });
                                    vcd_db.padding_value.push(padding_vec);
                                    vcd_db.var_value.push(zero_stamp_values);
                                } else {
                                    let mut padding_vec = vec![];
                                    let mut value_vec = vec![];
                                    res.1.iter().for_each(|item|{
                                        padding_vec.push(*vcd_db.var_id_map.get(item.1).expect(&format!("current id is {:?}, current var id map {:?}", item.1,vcd_db.var_id_map))); // TODO add sever fatal
                                        value_vec.push(item.0.clone());
                                    });
                                    vcd_db
                                        .var_value
                                        .push(value_vec);
                                    vcd_db.padding_value.push(padding_vec);
                                }
                                // 2.
                                buff.clear();
                                // 3.
                                buff.push(line.unwrap());
                            }
                        } else {
                        	// parse dump ctrl and not append buffer
                        	if timestamp_rcvd && check_skipped_after_timestamp(&leading_tag) {
                        		// TODO: dump ctrl should be skipped when it come along inside timestamp
                        		// TODO: comment should be skipped when it come along inside timestamp
                        	} else {
                        		buff.push(line.unwrap());
                        	}
                            let casted = buff.join("\n");
                            if check_end(&casted) || check_end_contained(&casted) {
                                // 1.reset state
                                statement_start_without_end = false;
                                // 2.get casted
                                let casted = buff.join("\n");
                                // 3.clear buff
                                buff.clear();
                                match leading_tag {
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
        }
    }
    Ok(vcd_db)
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
    fn peak_tag_test1() {
        assert_eq!(peak_tag("$date\n"), Some(VcdTag::Date));
        assert_eq!(peak_tag("$date\n\n"), Some(VcdTag::Date));
        assert_eq!(peak_tag("$date"), Some(VcdTag::Date));
        assert_eq!(peak_tag("$date today"), Some(VcdTag::Date));
    }

    #[test]
    fn test_vcd_variable_val_parser1() {
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
                    "#"
                )
            ))
        );
        assert_eq!(
            vcd_variable_val_parser("x&\n"),
            Ok(("\n", (VarValue::Scalar(ScalarValue::Xstate), "&")))
        );
    }

    #[test]
    fn test_vcd_variable_val_parser2() {
        assert_eq!(
            vcd_variable_val_parser("b1001 #"),
            Ok((
                "",
                (
                    VarValue::Vector(vec![
                        ScalarValue::ZeroOne(true),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(true)
                    ]),
                    "#"
                )
            ))
        );
        assert_eq!(
            vcd_variable_val_parser("x&\n"),
            Ok(("\n", (VarValue::Scalar(ScalarValue::Xstate), "&")))
        );
    }

    #[test]
    fn test_vcd_variable_val_parser3() {
        assert_eq!(
            vcd_variable_val_parser("b1001 #\nb1001 !"),
            Ok((
                "\nb1001 !",
                (
                    VarValue::Vector(vec![
                        ScalarValue::ZeroOne(true),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(false),
                        ScalarValue::ZeroOne(true)
                    ]),
                    "#"
                )
            ))
        );
        assert_eq!(
            vcd_variable_val_parser("x&\n"),
            Ok(("\n", (VarValue::Scalar(ScalarValue::Xstate), "&")))
        );
    }

    #[test]
    fn test_var_def_parser() {
        let plain1 = "$var wire 1 ! D0 $end";
        let plain2 = "$var wire 1 \" D1 $end";
        let plain3 = "$var reg 32 0 raddr [31:0] $end";

        assert_eq!(
            vcd_variable_def_parser(plain1),
            Ok((
                "",
                (
                    "!".to_string(),
                    Variable {
                        var_type: VarType::Wire,
                        name: "D0".to_string(),
                        width: 1
                    }
                )
            ))
        );
        assert_eq!(
            vcd_variable_def_parser(plain2),
            Ok((
                "",
                (
                    "\"".to_string(),
                    Variable {
                        var_type: VarType::Wire,
                        name: "D1".to_string(),
                        width: 1
                    }
                )
            ))
        );
        assert_eq!(
            vcd_variable_def_parser(plain3),
            Ok((
                "",
                (
                    "0".to_string(),
                    Variable {
                        var_type: VarType::Reg,
                        name: "raddr".to_string(),
                        width: 32
                    }
                )
            ))
        );
    }
    // pub struct Variable {
    //     var_type: VarType,
    //     name: String,
    //     width: u16,
    // }

    #[test]
    fn test_vcd_variable_parser1() {
        let plain = "#0 1! 0\" 0# 1$ b1100 &";
        let expected_result = vec![
            (VarValue::Scalar(ScalarValue::ZeroOne(true)), "!"),
            (VarValue::Scalar(ScalarValue::ZeroOne(false)), "\""),
            (VarValue::Scalar(ScalarValue::ZeroOne(false)), "#"),
            (VarValue::Scalar(ScalarValue::ZeroOne(true)), "$"),
            (
                VarValue::Vector(vec![
                    ScalarValue::ZeroOne(true),
                    ScalarValue::ZeroOne(true),
                    ScalarValue::ZeroOne(false),
                    ScalarValue::ZeroOne(false),
                ]),
                "&",
            ),
        ];
        assert_eq!(vcd_variable_parser(plain), Ok(("", (0, expected_result,))));
    }
    // "#0 1! 0" 0# 1$ b1100 &""

    #[test]
    fn test_vcd_variable_parser2() {
        let plain = "#0 bzzzzzz J\nbzzzzz =\nbzZZZZ >\n";
        let expected_result = vec![
            (
                VarValue::Vector(vec![
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                ]),
                "J",
            ),
            (
                VarValue::Vector(vec![
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                ]),
                "=",
            ),
            (
                VarValue::Vector(vec![
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                    ScalarValue::Zstate,
                ]),
                ">",
            ),
        ];
        assert_eq!(
            vcd_variable_parser(plain),
            Ok(("\n", (0, expected_result,)))
        );
    }

    // #[test]
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
