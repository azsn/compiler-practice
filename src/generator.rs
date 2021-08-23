use super::parser;
use std::collections::HashMap;
use std::str;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SymbolType {
	Int,
	Bool,
	Func,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
	map: HashMap<Vec<u8>, Symbol>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
	typ: SymbolType,

	// For function symbols only
	ret_type: Option<SymbolType>,
	arg_types: Vec<SymbolType>,
}

pub fn generate(program: &parser::ProgramNode) {
	let mut symbols = SymbolTable { ..Default::default() };

	symbols.map.insert(b"print_int".to_vec(), Symbol {
		typ: SymbolType::Func,
		ret_type: None,
		arg_types: vec!{SymbolType::Int},
	});

	symbols.map.insert(b"print_bool".to_vec(), Symbol {
		typ: SymbolType::Func,
		ret_type: None,
		arg_types: vec!{SymbolType::Bool},
	});

	println!("void print_int(int v) {{ printf(\"%d\", v); }}");
	println!("void print_bool(int v) {{ printf(\"%t\", v); }}");

	// Add all functions to global symbol table before generating rest of code so that
	// functions can be referenced out of order.
	for func in &program.functions {
		if symbols.map.contains_key(&func.name) {
			panic!("Generator: Duplicate function {}", str::from_utf8(&func.name).unwrap());
		}
		symbols.map.insert(func.name.clone(), Symbol {
			typ: SymbolType::Func,
			ret_type: if func.return_type.is_none() {
				None
			} else {
				Some(var_type_to_symbol_type(&func.return_type.unwrap()))
			},
			arg_types: get_arg_types(&func.args.args),
		});

		generate_func_header(func);
		println!(";");
	}
	println!();

	if let Some(main_func) = symbols.map.get(&b"main".to_vec()) {
		if main_func.ret_type != Some(SymbolType::Int) {
			panic!("Generator: Main function must have return type Int");
		}
		if main_func.arg_types.len() != 0 {
			panic!("Generator: Main function must take no args");
		}
	} else {
		panic!("Generator: Main function is not defined.");
	}

	for func in &program.functions {
		generate_func(&func, &symbols);
	}
}

fn var_type_to_symbol_type(var_type: &parser::VarType) -> SymbolType {
	match var_type {
		parser::VarType::Int => return SymbolType::Int,
		parser::VarType::Bool => return SymbolType::Bool,
	}
}

fn get_arg_types(args: &Vec<(parser::VarType, Vec<u8>)>) -> Vec<SymbolType> {
	let mut v = Vec::new();
	for (typ, _) in args {
		v.push(var_type_to_symbol_type(&typ));
	}
	return v;
}

fn generate_func(func: &parser::FuncNode, parent_symbols: &SymbolTable){
	let mut symbols = SymbolTable { ..Default::default() };

	if let Some(return_type) = func.return_type {
		// Fake symbol to help generate_return_node check if the return type is right
		// Use ~ to avoid conflicts since it is not possible with a real symbol
		symbols.map.insert(b"~return".to_vec(), Symbol {
			typ: if return_type == parser::VarType::Int {
				SymbolType::Int
			} else {
				SymbolType::Bool
			},
			ret_type: None,
			arg_types: vec!{},
		});
	}

	for (typ, name) in &func.args.args {
		if symbols.map.contains_key(name) {
			panic!("Generator: Duplicate argument name {} in function {}", str::from_utf8(&name).unwrap(), str::from_utf8(&func.name).unwrap());
		}
		symbols.map.insert(name.clone(), Symbol {
			typ: if typ == &parser::VarType::Int {
				SymbolType::Int
			} else {
				SymbolType::Bool
			},
			ret_type: None,
			arg_types: vec!{},
		});
	}

	generate_func_header(func);
	print!(" ");

	let s = vec!{parent_symbols, &symbols};
	generate_block(&func.body, &s);
	println!();
}

fn generate_func_header(func: &parser::FuncNode) {
	match func.return_type {
		Some(parser::VarType::Int) => print!("int"),
		Some(parser::VarType::Bool) => print!("int"),
		None => print!("void"),
	}
	print!(" {}(", str::from_utf8(&func.name).unwrap());
	if let Some((_, name)) = func.args.args.first() {
		print!("int {}", str::from_utf8(&name).unwrap());
		for (_, name) in &func.args.args[1..] {
			print!(", int {}", str::from_utf8(&name).unwrap());
		}
	}
	print!(")");
}

fn generate_block(block: &parser::BlockNode, parent_symbols: &Vec<&SymbolTable>) {
	let mut symbols = SymbolTable { ..Default::default() };

	println!("{{");

	for statement in &block.statements[..] {
		let mut s = parent_symbols.clone();
		s.append(&mut vec!{&symbols});

		if let Some(if_) = &statement.if_ {
			generate_if(&if_, &s);
		} else if let Some(while_) = &statement.while_ {
			generate_while(&while_, &s);
		} else if let Some(decl) = &statement.declaration {
			generate_declaration(&decl, &parent_symbols, &mut symbols);
		} else if let Some(assignment) = &statement.assignment {
			generate_assignment(&assignment, &s);
		} else if let Some(func_call) = &statement.func_call {
			generate_func_call(&func_call, &s);
			println!(";");
		} else if let Some(ret) = &statement.return_ {
			generate_return(ret, &s);
		}
	}

	print!("}}");
}

fn generate_if(if_: &parser::IfNode, symbols: &Vec<&SymbolTable>) {
	print!("if (");
	let cond_type = generate_expression(&if_.condition, symbols);
	if cond_type != SymbolType::Bool {
		panic!("Generator: Type of if statement condition must be bool");
	}

	print!(") ");

	generate_block(&if_.true_body, symbols);

	if let Some(false_body) = &if_.false_body {
		print!(" else ");
		generate_block(false_body, symbols);
	}

	println!();
}

fn generate_while(while_: &parser::WhileNode, symbols: &Vec<&SymbolTable>) {
	print!("while (");
	let cond_type = generate_expression(&while_.condition, symbols);
	if cond_type != SymbolType::Bool {
		panic!("Generator: Type of while statement condition must be bool");
	}

	print!(") ");
	generate_block(&while_.body, symbols);
	println!();
}

fn generate_declaration(decl: &parser::DeclarationNode, parent_symbols: &Vec<&SymbolTable>, symbols: &mut SymbolTable) {

	if symbols.map.contains_key(&decl.var) {
		panic!("Generator: Declaring duplicate variable {}", str::from_utf8(&decl.var).unwrap());
	}
	
	let sym_type = if decl.typ == parser::VarType::Int {
		SymbolType::Int
	} else {
		SymbolType::Bool
	};

	print!("int {} = ", str::from_utf8(&decl.var).unwrap());

	let mut s = parent_symbols.clone();
	s.append(&mut vec!{&symbols});
	let expr_type = generate_expression(&decl.value, &s);

	if expr_type != sym_type {
		panic!("Generator: Variable declaration specifies type {:?} but is given a value of type {:?}", sym_type, expr_type);
	}

	println!(";");

	symbols.map.insert(decl.var.clone(), Symbol {
		typ: sym_type,
		ret_type: None,
		arg_types: vec!{},
	});
}

fn generate_assignment(assignment: &parser::AssignmentNode, symbols: &Vec<&SymbolTable>) {
	let name = str::from_utf8(&assignment.var).unwrap();
	let sym = lookup_symbol(symbols, &assignment.var);

	if sym.is_none() {
		panic!("Generator: Assignment to undefined variable {}", name);
	}

	let sym = sym.unwrap();

	print!("{} = ", name);
	let value_typ = generate_expression(&assignment.value, symbols);

	if value_typ != sym.typ {
		panic!("Generator: Assignment to {} expected type {:?}, got type {:?}", name, sym.typ, value_typ);
	}

	println!(";");
}

fn generate_return(ret: &parser::ReturnNode, symbols: &Vec<&SymbolTable>) {
	let expected_ret_type = lookup_symbol(symbols, &b"~return".to_vec());
	if let Some(value) = &ret.value {
		if expected_ret_type.is_none() {
			panic!("Generator: Expected no return value");
		}

		print!("return ");
		let ret_type = generate_expression(value, symbols);
		if ret_type != expected_ret_type.unwrap().typ {
		//if (ret_type == parser::VarType::Int && expected_ret_type.unwrap().typ != SymbolType::Int) || (ret_type == parser::VarType::Bool && expected_ret_type.unwrap().typ != SymbolType::Bool) {
			panic!("Generator: Expected return value of type {:?}, got {:?}", expected_ret_type.unwrap().typ, ret_type);
		}
		println!(";");
	} else {
		if let Some(typ) = expected_ret_type {
			panic!("Generator: Expected return value of type {:?}", typ.typ);
		}

		println!("return;");
	}
}

fn generate_expression(expr: &parser::ExprNode, symbols: &Vec<&SymbolTable>) -> SymbolType {
	let left_type = generate_val(&expr.left, symbols);

	if let Some((op, right)) = &expr.right {
		if left_type == SymbolType::Bool {
			if op != &parser::OpType::Equals && op != &parser::OpType::NotEquals {
				panic!("Generator: Arithmatic operator {:?} used on boolean in an expression.", op);
			}
		}

		match op {
			parser::OpType::Plus => print!(" + "),
			parser::OpType::Minus => print!(" - "),
			parser::OpType::Multiply => print!(" * "),
			parser::OpType::Divide => print!(" / "),
			parser::OpType::GreaterThan => print!(" > "),
			parser::OpType::LessThan => print!(" < "),
			parser::OpType::Equals => print!(" == "),
			parser::OpType::NotEquals => print!(" != "),
		}

		let right_type = generate_val(&right, symbols);

		if right_type != left_type {
			panic!("Generator: Type of right side of expression does not match type of left.");
		}
	}

	return left_type;
}

fn generate_val(val: &parser::ValNode, symbols: &Vec<&SymbolTable>) -> SymbolType {
	if let Some(var) = &val.var {
		if let Some(sym) = lookup_symbol(symbols, &var) {
			print!("{}", str::from_utf8(&var).unwrap());
			return sym.typ;
		}
		panic!("Generator: Reference to unknown variable {}", str::from_utf8(&var).unwrap());
	} else if let Some(number) = val.number {
		print!("{}", number);
		return SymbolType::Int;
	} else if let Some(bool_) = val.bool_ {
		print!("{}", if bool_ { 1 } else { 0 });
		return SymbolType::Bool;
	} else if let Some(func_call) = &val.func_call {
		let ret_type = generate_func_call(&func_call, symbols);
		if ret_type.is_none() {
			panic!("Generator: Function with void return value used in expression");
		}
		return ret_type.unwrap();
	} else if let Some(expr) = &val.expr {
		print!("(");
		let typ = generate_expression(&expr, symbols);
		print!(")");
		return typ;
	} else {
		panic!("Generator: Invalid ValNode");
	}
}

fn generate_func_call(func_call: &parser::FuncCallNode, symbols: &Vec<&SymbolTable>) -> Option<SymbolType> {
	let name = str::from_utf8(&func_call.name).unwrap();
	if let Some(sym) = lookup_symbol(symbols, &func_call.name) {
		if sym.typ != SymbolType::Func {
			panic!("Generator: Call to non-function {}", name);
		}

		if sym.arg_types.len() != func_call.params.params.len() {
			panic!("Generator: Function call to {} expected {} parameters, got {}", name, sym.arg_types.len(), func_call.params.params.len());
		}

		print!("{}(", name);
		if let Some(first_param) = func_call.params.params.first() {
			let param_type = generate_expression(first_param, symbols);
			if sym.arg_types[0] != param_type {
				panic!("Generator: Parameter 1 to function call {} should be type {:?}, got type {:?}", name, sym.arg_types[0], param_type);
			}

			let mut i = 1;
			for param in &func_call.params.params[1..] {
				print!(", ");
				let param_type = generate_expression(param, symbols);
				if sym.arg_types[i] != param_type {
					panic!("Generator: Parameter {} to function call {} should be type {:?}, got type {:?}", i+1, name, sym.arg_types[i], param_type);
				}

				i += 1;
			}
		}

		print!(")");
		return sym.ret_type;
	}

	panic!("Generator: Call to unknown function {}", str::from_utf8(&func_call.name).unwrap());
}

fn lookup_symbol<'a>(symbols: &Vec<&'a SymbolTable>, key: &Vec<u8>) -> Option<&'a Symbol> {
	for table in symbols.iter().rev() {
		if let Some(val) = table.map.get(key) {
			return Some(val);
		}
	}
	return None;
}
