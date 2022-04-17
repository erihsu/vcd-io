use nom::{
    branch::alt,
    bytes::complete::{tag,take_until1,take_until},
    combinator::{map,map_res,opt,recognize,value},
    sequence::{delimited, preceded, tuple,pair},
    character::complete::{one_of,alpha1,digit1,char,space1,alphanumeric1,multispace0},
    multi::{many0,many1},
    Finish,
};


use std::cell::RefCell;

use crate::*;
use crate::error::VcdError;


use std::str;


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



pub(crate) fn vcd_header_parser(input:&str) -> VcdRes<&str,(&str,&str,&str)>{
	tuple((
		map(delimited(ws(tag("$date")),take_until1("$"),tag("$end")),str::trim),
		map(delimited(ws(tag("$version")),take_until1("$"),tag("$end")),str::trim),
		map(delimited(ws(tag("$comment")),take_until1("$"),tag("$end")),str::trim),
		))(input)
}

pub(crate) fn vcd_timescale_parser(input:&str) -> VcdRes<&str,(u32,TimeUnit)>{
	delimited(
		ws(tag("$timescale")),
		ws(tuple((
			map(digit1,|s:&str|s.parse().unwrap()),
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
	map(many1(one_of(SPECIAL_IDENTIFIER)),|res|res.iter().collect())(input)
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
				digit1,
				space1,
				vcd_identifier_parser,
				space1,
				tstring,
				take_until("$"),
			)),
			ws(tag("$end")),
		),
		|res:(VarType,&str,&str,&str,_,&str,&str,&str)|(res.4,Variable{var_type:res.0,name:res.6.to_string(),width:res.2.parse().unwrap()})
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
				map(tag("fork"),|_|ScopeType::Fork),
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


pub(crate) fn vcd_scalar_val_parser(input:&str) -> VcdRes<&str,ScalarValue> {
	ws(alt((
	map(recognize(char('0')),|_|ScalarValue::ZeroOne(false)),
	map(recognize(char('1')),|_|ScalarValue::ZeroOne(true)),
	map(recognize(char('x')),|_|ScalarValue::Xstate),
	map(recognize(char('X')),|_|ScalarValue::Xstate),
	)))(input)
}

pub(crate) fn vcd_vector_val_parser(input:&str) -> VcdRes<&str,Vec<ScalarValue>> {
	preceded(
		ws(alt((char('b'),char('B')))),
		many1(
			alt((
				map(char('0'),|_|ScalarValue::ZeroOne(false)),
				map(char('1'),|_|ScalarValue::ZeroOne(true)),
				map(char('x'),|_|ScalarValue::Xstate),
				map(char('X'),|_|ScalarValue::Xstate),
			))
			)
		)(input)
}

pub(crate) fn vcd_real_val_parser(input:&str) -> VcdRes<&str,String> {
	map(preceded(
		ws(alt((char('R'),char('r')))),
		digit1,
		),|res:&str|res.to_string())(input)
}

pub(crate) fn vcd_variable_val_parser(input:&str) -> VcdRes<&str,(VarValue,&str)> {
	pair(
		alt((
			map(vcd_scalar_val_parser,|v|VarValue::Scalar(v)),
			map(vcd_vector_val_parser,|v|VarValue::Vector(v)),
			map(vcd_real_val_parser,|v|VarValue::Real(v)),

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


pub(crate) fn vcd_parser(input:&str) -> std::result::Result<VcdDb,VcdError> {
	let scopes:RefCell<Vec<Scope>> = RefCell::new(vec![]);
	
	let mut variables: Vec<Variable> = vec![];

	let mut values:Vec<Vec<VarValue>> = vec![];	

	let mut var_id_map:HashMap<String,VariableIndex> = HashMap::new();
	let value_var_map:RefCell<HashMap<ValueIndex,String>> = RefCell::new(HashMap::new());
	let mut timestaps:Vec<u32> = vec![];

	let scope_index_chain:RefCell<Vec<ScopeIndex>> = RefCell::new(vec![]);
	let mut scope_idx:usize = 0;
	let mut var_idx:usize = 0;
	let has_dumpvars = RefCell::new(false);


	let (_,((date,version,comment),timescale,_,_,_,_,_)) = tuple((
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
					scopes.borrow_mut().push(a_scope);

					scope_index_chain.borrow_mut().push(scope_idx);
					scope_idx += 1;
				}),
				map(vcd_variable_def_parser,|res:(String,Variable)|{
					variables.push(res.1);
					var_id_map.insert(res.0,var_idx);
					scopes.borrow_mut()[*scope_index_chain.borrow().last().unwrap()].variables.push(var_idx);
					var_idx += 1;
					
				}),
				map(vcd_upscope_parser,|_| {
					let _ = scope_index_chain.borrow_mut().pop();
				})
			))
		),
		ws(tag("$enddefinitions")),
		ws(tag("$end")),
		map(opt(vcd_dumpvars_parser
		),|d|{
			if let Some(res) = d {
				*has_dumpvars.borrow_mut() = true;
				for (i,(_,s)) in res.iter().enumerate() {
					value_var_map.borrow_mut().insert(i,s.to_string());
				}
		
			}

		}),
		map(many1(vcd_variable_parser),|res|{
				if *has_dumpvars.borrow() {
					for (t,v) in res {
						timestaps.push(t);
						values.push(v.iter().map(|x|x.0.clone()).collect());
					}
				} else {
					for (i,(_,v)) in res.iter().enumerate() {
						if i == 0 {
							let mut zero_stamp_values:Vec<VarValue> = vec![];
							for (j,(a_v,s)) in v.iter().enumerate() {
								zero_stamp_values.push(a_v.clone());
								value_var_map.borrow_mut().insert(j,s.to_string());
							}
							values.push(zero_stamp_values);
						} else {
							for (t,v) in &res {
								timestaps.push(*t);
								values.push(v.iter().map(|x|x.0.clone()).collect());
							}
						}
					}
				}
		})
		
	))(input).finish()/*.map_err(|_|VcdError::BadVCD)*/.unwrap();

	return Ok(VcdDb{
		date:date.to_string(),
		version:version.to_string(),
		comment:comment.to_string(),
		timescale,
		timestap:timestaps,
		scope:scopes.take(),
		variable:variables,
		var_value:values,
		var_id_map,
		value_var_map:value_var_map.take(),
	});


}

#[cfg(test)]
mod test {
	use super::*;
	#[test]
	fn test_anychar() {
		assert_eq!(vcd_identifier_parser("abcd_efg"),Ok(("","abcd_efg".to_string())));
		assert_eq!(vcd_identifier_parser("abcd efg"),Ok((" efg","abcd".to_string())));
		assert_eq!(vcd_identifier_parser("#* abcd"),Ok((" abcd","#*".to_string())));

	}
	#[test]
	fn test_vcd_variable_val_parser() {
		assert_eq!(vcd_variable_val_parser("b1001 #\n"),Ok(("\n",(VarValue::Vector(vec![ScalarValue::ZeroOne(true),ScalarValue::ZeroOne(false),ScalarValue::ZeroOne(false),ScalarValue::ZeroOne(true)])," #"))));
		assert_eq!(vcd_variable_val_parser("x&\n"),Ok(("\n",(VarValue::Scalar(ScalarValue::Xstate),"&"))));
	}

	#[test]
	fn test_vcd_parser() {
		let vcd_plain = concat!(
						"$date\nDate text. For example: November 11, 2009.\n$end",
						"$version\nVCD generator tool version info text.\n$end",
						"$comment\nAny comment text.\n$end",
						"$timescale 1ps $end",
						"$scope module logic $end",
						"$var wire 8 # data $end",
						"$var wire 1 $ data_valid $end",
						"$var wire 1 % en $end",
						"$var wire 1 & rx_en $end",
						"$var wire 1 ' tx_en $end",
						"$var wire 1 ( empty $end",
						"$var wire 1 ) underrun $end",
						"$upscope $end",
						"$enddefinitions $end",
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
		let vcd_db:VcdDb = vcd_parser(&vcd_plain).unwrap();
		assert_eq!(vcd_db.date,"Date text. For example: November 11, 2009.".to_string());
		assert_eq!(vcd_db.comment,"Any comment text.".to_string());
		assert_eq!(vcd_db.version,"VCD generator tool version info text.".to_string());
		assert_eq!(vcd_db.timescale.1,TimeUnit::Psec);
		assert_eq!(vcd_db.variable[0],Variable{var_type:VarType::Wire,name:"data".to_string(),width:8});
		assert_eq!(vcd_db.var_id_map.get("#"),Some(&0));
		
	}

}