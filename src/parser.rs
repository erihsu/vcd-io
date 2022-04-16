use nom::{
    branch::alt,
    bytes::complete::{tag,take_until1,take_until},
    combinator::{map,map_res,opt,recognize,value},
    error::context,
    sequence::{delimited, preceded, tuple,pair,terminated},
    character::complete::{hex_digit1,alpha1,digit1,char,anychar,space1,alphanumeric1,multispace0},
    multi::{many0,many1},
    Finish,
};

use crate::*;
use crate::error::VcdError;


use std::str;
use std::str::FromStr;

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



pub(crate) fn vcd_header_parser(input:&str) -> VcdRes<&str,(&str,&str,&str)>{
	tuple((
		delimited(ws(tag("$date")),take_until1("$"),tag("$end")),
		delimited(ws(tag("$version")),take_until1("$"),tag("$end")),
		delimited(ws(tag("$comment")),take_until1("$"),tag("$end")),
		))(input)
}

pub(crate) fn vcd_timescale_parser(input:&str) -> VcdRes<&str,(u32,TimeUnit)>{
	delimited(
		ws(tag("$timescale")),
		ws(tuple((
			map(digit1,str::parse),
			alt((map(tag("ps"),|_|TimeUnit::Psec),
				map(char('s'),|_|TimeUnit::Sec),
				map(tag("ms"),|_|TimeUnit::Msec),
				map(tag("ns"),|_|TimeUnit::Nsec)
			)),
		))),
		ws(tag("$end")),
		)(input)
}

pub(crate) fn vcd_identifier_parser(input:&str) -> VcdRes<&str,String> {
	map(many1(anychar),|res:Vec<char>|res.into_iter().collect())(input)
}


pub(crate) fn vcd_variable_def_parser(input:&str) -> VcdRes<&str,(String,Variable)> {
	map(
		delimited(
			ws(tag("$var")),
			tuple((
				alt((
					map(tag("wire"),|_|VarType::Wire),
					map(tag("reg"),|_|VarType::Reg),
				)),
				space1,
				map_res(digit1,str::parse),
				space1,
				map(many1(anychar),|res:Vec<char>|res.into_iter().collect()),
				space1,
				tstring,
				take_until("$"),
			)),
			ws(tag("$end")),
		),
		|res:(VarType,_,u16,_,String,_,String,_)|(res.4,Variable{var_type:res.0,name:res.6,width:res.2})
	)(input)
}

pub(crate) fn vcd_scope_def_parser(input:&str) -> VcdRes<&str,(ScopeType,&str)> {
	delimited(
		ws(tag("$scope")),
		tuple((
			alt((
				map(tag("module"),|_|ScopeType::Module),
				map(tag("task"),|_|ScopeType::Task),
				map(tag("function"),|_|ScopeType::Function),
				map(tag("Fork"),|_|ScopeType::Fork),
				)),
			tstring,
			)),
		ws(tag("$end"))
		)(input)
}


pub(crate) fn vcd_upscope_parser(input:&str) -> VcdRes<&str,()> {
	value(
		(),
	pair(ws(tag("$upscope")),tag("$end"))
	)(input)
}

pub(crate) fn vcd_variable_val_parser(input:&str) -> VcdRes<&str,(VarValue,&str)> {
	pair(
		alt((
			alt((
				map(recognize(char('0')),|_|ScalarValue::ZeroOne(false)),
				map(recognize(char('1')),|_|ScalarValue::ZeroOne(true)),
				map(recognize(char('x')),|_|ScalarValue::Xstate),
				map(recognize(char('X')),|_|ScalarValue::Xstate),
				)),
			map_res(
				delimited(alt((char('b'),char('B'))),
				many1(
					alt((
						map(char('0'),|_|ScalarValue::ZeroOne(false)),
						map(char('1'),|_|ScalarValue::ZeroOne(true)),
						map(char('x'),|_|ScalarValue::Xstate),
						map(char('X'),|_|ScalarValue::Xstate),
					))),space1),|res|VarValue::Vector(res)),
			map_res(
				delimited(
					alt((char('R'),char('r'))),
					many1(hex_digit1),
					space1,
					),
				|res:Vec<char>|res.into_iter().collect()
				)

		)),take_until1("\n")
	)(input)
}

pub(crate) fn vcd_timestap_parser(input:&str) -> VcdRes<&str,u32> {
	preceded(ws(char('#')),map_res(digit1,str::parse))(input)
}

pub(crate) fn vcd_variable_parser(input:&str) -> VcdRes<&str,(u32,Vec<(VarValue,&str)>)> {
	tuple((
		vcd_timestap_parser,
		many1(vcd_variable_val_parser),
		))(input)
}

pub(crate) fn vcd_dumpvars_parser(input:&str) -> VcdRes<&str,Vec<(VarValue,&str)>> {
	delimited(ws(tag("$dumpvars")),many1(vcd_variable_val_parser),ws(tag("$end")))(input)
}


pub(crate) fn vcd_parser(input:&str) -> std::result::Result<Vcd_Db,VcdError> {
	let mut scopes:Vec<Scope> = vec![];
	
	let mut variables: Vec<Variable> = vec![];

	let mut values:Vec<Vec<VarValue>> = vec![];	

	let mut var_id_map:HashMap<String,VariableIndex> = HashMap::new();
	let mut value_var_map:HashMap<ValueIndex,String> = HashMap::new();
	let mut timestaps:Vec<u32> = vec![];

	let mut scope_index_chain:Vec<ScopeIndex> = vec![];
	let mut value_bf:Vec<VarValue> = vec![];
	let mut scope_idx:usize = 0;
	let mut var_idx:usize = 0;
	let mut has_dumpvars:bool = false;


	let (((date,version,comment),timescale,_,_,_,_,_),_) = tuple((
		vcd_header_parser,
		vcd_timescale_parser,
		many1(
			alt((
				map(vcd_scope_def_parser,|res:(ScopeType,&str)|{
					let a_scope:Scope = Scope{
						scope_type:res.0,
						scope_name:res.1.to_string(),
						sub_scope_idx:vec![],
						variables: vec![],
					};
					scopes.push(a_scope);

					scope_index_chain.push(scope_idx);
					scope_idx += 1;
				}),
				map(vcd_variable_def_parser,|res:(String,Variable)|{
					variables.push(res.1);
					var_id_map.insert(res.0,var_idx);
					scopes[scope_index_chain.last().unwrap()].variables.push(var_idx);
					var_idx += 1;
					
				}),
				map(vcd_upscope_parser,|_| {
					let _ = scope_index_chain.pop();
				})
			))
		),
		ws(tag("$enddefinitions")),
		ws(tag("$end")),
		opt(map(vcd_dumpvars_parser,|res|{
			for (i,(v,s)) in res.iter().enumerate() {
				value_var_map.insert(i,s.to_string());
			}
		})),
		map(many1(vcd_variable_parser),|res|{
				if has_dumpvars {
					for (t,v) in res {
						timestaps.push(t);
						values.push(v.iter().map(|x|x.0).collect());
					}
				} else {
					for (i,(t,v)) in res.iter().enumerate() {
						if i == 0 {
							let mut zero_stamp_values = vec![];
							for (j,(a_v,s)) in v.iter().enumerate() {
								zero_stamp_values.push(a_v);
								value_var_map.insert(j,s.to_string());
							}
							values.push(zero_stamp_values);
						} else {
							for (t,v) in res {
								timestaps.push(t);
								values.push(v.iter().map(|x|x.0).collect());
							}
						}
					}
				}
		})
		
	))(input).finish().map_err(|_|Err(VcdError::BadVCD))?;

	return Ok(Vcd_Db{
		date:date.to_string(),
		version:version.to_string(),
		comment:comment.to_string(),
		timescale,
		timestap:timestaps,
		scope:scopes,
		variable:variables,
		var_value:values,
		var_id_map,
		value_var_map,
	});


}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn test_anychar() {
		assert_eq!(vcd_identifier_parser("abcd_efg"),Ok(("","abcd_efg")));
		assert_eq!(vcd_identifier_parser("abcd efg"),Ok((" efg","abcd")));
	}
	#[test]
	fn test_vcd_variable_val_parser() {
		assert_eq!(vcd_variable_val_parser("b1001 #\n"),Ok(("",(VarValue::Vector(vec![ScalarValue::ZeroOne(true),ScalarValue::ZeroOne(false),ScalarValue::ZeroOne(false),ScalarValue::ZeroOne(true)]),"#"))));
		assert_eq!(vcd_variable_val_parser("x&\n"),Ok(("",(VarValue::Scalar(ScalarValue::Xstate),"&"))));
	}

}