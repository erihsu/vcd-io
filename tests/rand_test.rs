#[test]
#[cfg(feature = "dev")]
fn rand_vcddb_test() {
	use rand::prelude::*;
	use rand::distributions::{Standard};
	use vcd_io::*;
	use std::iter;
	use random_string::generate;

	let charset = "1234567890abcdefgJKLMMMUVvw";

	let mut vcd_db = VcdDb::new();
	let time_scale = 10u32;
	vcd_db.timescale = (time_scale,TimeUnit::Psec);
	let mut rng = rand::thread_rng();
	let mut rng2 = rand::thread_rng();

    let scalar_val_num: usize = 20;
    let vector_val_num: usize = 30;
    const VECTOR_WIDTH: usize = 12;
    let total_var_num = scalar_val_num + vector_val_num;
    let mut shuffled_index:Vec<usize> = (0..total_var_num).collect();
    shuffled_index.shuffle(&mut rng);
    let iteration: usize = 20;
    let width_list:Vec<u16> = iter::repeat(1).take(scalar_val_num).chain(iter::repeat(VECTOR_WIDTH as u16).take(vector_val_num)).collect();


    vcd_db.timestap = (0..iteration).map(|i|i as u32 * time_scale).collect();
    vcd_db.variable = (0..total_var_num).into_iter().map(|i| {
    	let name = generate(6, charset);
    	let width = width_list[shuffled_index[i]];
    	let var_type = rng.gen::<VarType>();
    	vcd_db.var_id_map.insert(name.clone(),i);
    	Variable {
		    var_type,
		    name,
		    width,
    	}
    }).collect();

    let mut iter_array: Vec<_> = (0..VECTOR_WIDTH).into_iter()
    				.map(|_|{
    					let cloned_rng = rng.clone();
    					Standard.sample_iter(cloned_rng).take(iteration * vector_val_num)})
    				.collect();

    for i in 0..iteration {
    	vcd_db.var_value.push(vec![]);
    	vcd_db.padding_value.push(vec![]);
	    let mut scalars: Vec<VarValue> = Standard.sample_iter(&mut rng2).take(scalar_val_num).map(|s|VarValue::Scalar(s)).collect();
	    let vectors: Vec<VarValue> = (0..vector_val_num).into_iter().map(|_|{
	    	let mut vector:Vec<ScalarValue> = Vec::new();
	    	for v in &mut iter_array {
	    		vector.push(v.next().unwrap());
	    	}
	    	VarValue::Vector(vector)
	    }).collect();

	    // append vector var_value into scalar
	    scalars.extend_from_slice(&vectors);

	    for idx in &shuffled_index {
	    	let var_value_change_choice = rng.gen::<bool>();
	    	if i == 0 {
	    		vcd_db.padding_value[i].push(*idx);
	    		vcd_db.var_value[i].push(scalars[*idx].clone());
	    	} else {
		    	if !var_value_change_choice {
		    		vcd_db.padding_value[i].push(*idx);
		    	} else {
		    		vcd_db.var_value[i].push(scalars[*idx].clone());
		    	}
	    	}

	    	
	    }

    }

    assert_eq!(vcd_db.aligned(),false);

    assert_eq!(vcd_db.variable.len(),total_var_num);
    assert_eq!(vcd_db.var_value.len(),iteration);
    assert_eq!(vcd_db.var_id_map.len(),50);

    assert!(vcd_db.align_var_value());

    assert_eq!(vcd_db.aligned(),true);


}