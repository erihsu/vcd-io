use crate::{*,error::VcdError};
use std::{io::{BufWriter,Write},path::Path};
fn write_var_def(vcd:&Vcd_Db, scope:&Scope, w:&mut Vec<u8>) -> std::result::Result<(),VcdError>{
	writeln!(&mut w,"&scope {} {} &end",scope.scope_type,scope.scope_name)?;
	for var_idx in &scope.variables {
		let cur_var = vcd.variable[*var_idx];
		writeln!(&mut w,"$var {} {} {} $end",cur_var.var_type,cur_var.width,cur_var.name)?;
	}
	if scope.sub_scope_idx.len() == 0 {
		writeln!(&mut w,"$upscope $end")?;
	} else {
		for s_idx in &scope.sub_scope_idx {
			let sub_scope = vcd.scope[*s_idx];
			write_var_def(vcd,&sub_scope,w)?;
		}
	}
	Ok(())
}


impl Vcd_Db {
	pub fn save_vcd<P:AsRef<Path>>(&self,file:P) -> std::result::Result<(),VcdError>{
		let mut file_buffer = BufWriter::new(std::fs::File::create(file)?);
		let mut w:Vec<u8> = vec![];
		writeln!(&mut w,"$date {} $end",self.date)?;
		writeln!(&mut w,"$version {} $end",self.version)?;
		writeln!(&mut w,"$comment {} $end",self.comment)?;
		writeln!(&mut w,"$timescale {}{} $end",self.timescale.0,self.timescale.1)?;
		let top_scope:Scope = self.scope[0];
		write_var_def(&self,&top_scope,&mut w)?;


		for (i,t) in self.timestap.into_iter().enumerate() {
			writeln!(&mut w,"#{}",t)?;
			for v in &self.var_value {
				let id:&str = self.value_var_map.get(&i).unwrap();
				if let VarValue::Scalar(_) = v[i] {
					writeln!(&mut w,"{}{}",v[i],id)?;
				} else {
					writeln!(&mut w,"{} {}",v[i],id)?;
				}
				
			}
		}

		file_buffer.write_all(&w)?;
		file_buffer.flush()?;
		Ok(())
	}
}