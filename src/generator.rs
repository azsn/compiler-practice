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

	index: usize,

	// For function symbols only
	ret_type: Option<SymbolType>,
	arg_types: Vec<SymbolType>,
}

pub fn generate(program: &parser::ProgramNode) {
	let mut symbols = SymbolTable { ..Default::default() };
	let mut symbol_index: usize = 1;

	symbols.map.insert(b"print_int".to_vec(), Symbol {
		typ: SymbolType::Func,
		index: symbol_index,
		ret_type: None,
		arg_types: vec!{SymbolType::Int},
	});
	symbol_index += 1;

	symbols.map.insert(b"print_bool".to_vec(), Symbol {
		typ: SymbolType::Func,
		index: symbol_index,
		ret_type: None,
		arg_types: vec!{SymbolType::Bool},
	});
	symbol_index += 1;


	// #include <stdio.h>
	// void print_int1(int v) { printf(\"%d\\n\", v); }
	// void print_bool2(int v) { if (v) printf(\"true\\n\"); else printf(\"false\\n\"); }
	println!(
r#"@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"true\0A\00", align 1
@.str.2 = private unnamed_addr constant [7 x i8] c"false\0A\00", align 1

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @print_int1(i32) #0 {{
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %3)
  ret void
}}

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @print_bool2(i1) #0 {{
  %2 = alloca i1, align 4
  store i1 %0, i1* %2, align 4
  %3 = load i1, i1* %2, align 4
  %4 = icmp ne i1 %3, 0
  br i1 %4, label %5, label %7

; <label>:5:                                      ; preds = %1
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i32 0, i32 0))
  br label %9

; <label>:7:                                      ; preds = %1
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.2, i32 0, i32 0))
  br label %9

; <label>:9:                                      ; preds = %7, %5
  ret void
}}
"#);

	// Add all functions to global symbol table before generating rest of code so that
	// functions can be referenced out of order.
	for func in &program.functions {
		if symbols.map.contains_key(&func.name) {
			panic!("Generator: Duplicate function {}", str::from_utf8(&func.name).unwrap());
		}
		symbols.map.insert(func.name.clone(), Symbol {
			typ: SymbolType::Func,
			index: symbol_index,
			ret_type: if func.return_type.is_none() {
				None
			} else {
				Some(vartype2symtype(&func.return_type.unwrap()))
			},
			arg_types: get_arg_types(&func.args.args),
		});
		symbol_index += 1;
	}

	if let Some(main_func) = symbols.map.get(&b"main".to_vec()) {
		if main_func.ret_type != Some(SymbolType::Int) {
			panic!("Generator: Main function must have return type Int");
		}
		if main_func.arg_types.len() != 0 {
			panic!("Generator: Main function must take no args");
		}

		// int main(int argc, char **argv) { return mainXXX(); }
		println!(
r#"; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main(i32, i8**) #0 {{
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8**, align 8
  store i32 0, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  store i8** %1, i8*** %5, align 8
  %6 = call i32 @main{}()
  ret i32 %6
}}
"#, main_func.index);
	} else {
		panic!("Generator: Main function is not defined.");
	}

	for func in &program.functions {
		generate_func(&func, &symbols, &mut symbol_index);
	}
}

fn vartype2symtype(var_type: &parser::VarType) -> SymbolType {
	match var_type {
		parser::VarType::Int => return SymbolType::Int,
		parser::VarType::Bool => return SymbolType::Bool,
	}
}

fn get_arg_types(args: &Vec<(parser::VarType, Vec<u8>)>) -> Vec<SymbolType> {
	let mut v = Vec::new();
	for (typ, _) in args {
		v.push(vartype2symtype(&typ));
	}
	return v;
}

fn generate_func(func: &parser::FuncNode, parent_symbols: &SymbolTable, symbol_index: &mut usize){
	let mut symbols = SymbolTable { ..Default::default() };

	if let Some(return_type) = func.return_type {
		// Fake symbol to help generate_return_node check if the return type is right
		// Use ~ to avoid conflicts since it is not possible with a real symbol
		symbols.map.insert(b"~return".to_vec(), Symbol {
			typ: vartype2symtype(&return_type),
			index: *symbol_index,
			ret_type: None,
			arg_types: vec!{},
		});
		*symbol_index += 1;
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
			index: *symbol_index,
			ret_type: None,
			arg_types: vec!{},
		});
		*symbol_index += 1;
	}

	let s = vec!{parent_symbols, &symbols};

	generate_func_header(func, &s, true);
	println!(" {{");

	for (typ, name) in &func.args.args {
		let sym = lookup_symbol(&s, &name).unwrap();
		let size = if *typ == parser::VarType::Bool { 1 } else { 32 };
		let sname = str::from_utf8(&name).unwrap();
		println!("  %{}{} = alloca i{}", sname, sym.index, size);
		println!("  store i{} %arg_{}{}, i{}* %{}{}", size, sname, sym.index, size, sname, sym.index);
	}

	generate_block(&func.body, &s, symbol_index);

	// Making sure every code path returns a value sounds hard. ALso LLVM IR requires the last
	// instruction of a function which returns a value to be a `ret`, but even if every code
	// path returns a value, there could still be (unreachable) code after the last return
	// which will cause LLVM IR to error out. So just always return 0/false as the last
	// instruction.
	if let Some(return_type) = func.return_type {
		let typ = vartype2symtype(&return_type);
		let size = symtype2size(typ);
		println!("  ret i{} 0", size);
	} else {
		println!("  ret void");
	}

	println!("}}");
	println!();
}

fn generate_func_header(func: &parser::FuncNode, symbols: &Vec<&SymbolTable>, def: bool) {
	let sym = lookup_symbol(symbols, &func.name).unwrap();

	print!("define ");
	match func.return_type {
		Some(parser::VarType::Int) => print!("i32"),
		Some(parser::VarType::Bool) => print!("i1"),
		None => print!("void"),
	}

	print!(" @{}{}(", str::from_utf8(&func.name).unwrap(), sym.index);

	if let Some((typ, name)) = func.args.args.first() {
		let size = if *typ == parser::VarType::Bool { 1 } else { 32 };
		print!("i{}", size);
		if def {
			let sym = lookup_symbol(symbols, &name).unwrap();
			print!(" %arg_{}{}", str::from_utf8(&name).unwrap(), sym.index);
		}
		for (typ, name) in &func.args.args[1..] {
			let size = if *typ == parser::VarType::Bool { 1 } else { 32 };
			print!(", i{}", size);
			if def {
				let sym = lookup_symbol(symbols, &name).unwrap();
				print!(" %arg_{}{}", str::from_utf8(&name).unwrap(), sym.index);
			}
		}
	}
	print!(")");
}

fn generate_block(block: &parser::BlockNode, parent_symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) {
	let mut symbols = SymbolTable { ..Default::default() };

	for statement in &block.statements[..] {
		let mut s = parent_symbols.clone();
		s.append(&mut vec!{&symbols});

		if let Some(if_) = &statement.if_ {
			generate_if(&if_, &s, symbol_index);
		} else if let Some(while_) = &statement.while_ {
			generate_while(&while_, &s, symbol_index);
		} else if let Some(decl) = &statement.declaration {
			generate_declaration(&decl, &parent_symbols, &mut symbols, symbol_index);
		} else if let Some(assignment) = &statement.assignment {
			generate_assignment(&assignment, &s, symbol_index);
		} else if let Some(func_call) = &statement.func_call {
			generate_func_call(&func_call, &s, symbol_index);
		} else if let Some(ret) = &statement.return_ {
			generate_return(ret, &s, symbol_index);
		}
	}
}

fn generate_if(if_: &parser::IfNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) {
	let cond_type = generate_expression(&if_.condition, symbols, symbol_index);
	if cond_type != SymbolType::Bool {
		panic!("Generator: Type of if statement condition must be bool");
	}

	let cond_temp = *symbol_index - 1;
	let if_label = *symbol_index;
	let else_label = *symbol_index + 1;
	let mut end_label = *symbol_index + 2;
	*symbol_index += 3;

	if if_.false_body.is_none() {
		end_label = else_label;
		*symbol_index -= 1;
	}

	println!("  br i1 %t{}, label %t{}, label %t{}", cond_temp, if_label, else_label);
	println!();
	println!("t{}:", if_label);

	generate_block(&if_.true_body, symbols, symbol_index);
	println!("  br label %t{}", end_label);

	if let Some(false_body) = &if_.false_body {
		println!();
		println!("t{}:", else_label);
		generate_block(false_body, symbols, symbol_index);
		println!("  br label %t{}", end_label);
	}

	println!();
	println!("t{}:", end_label);
}

fn generate_while(while_: &parser::WhileNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) {
	let loop_cond = *symbol_index;
	println!();
	println!("  br label %loop_cond{}", loop_cond);
	println!("loop_cond{}:", loop_cond);
	*symbol_index += 1;

	let cond_type = generate_expression(&while_.condition, symbols, symbol_index);
	if cond_type != SymbolType::Bool {
		panic!("Generator: Type of while statement condition must be bool");
	}

	let cond_temp = *symbol_index - 1;

	let loop_start = *symbol_index;
	*symbol_index += 1;
	let loop_end = *symbol_index;
	*symbol_index += 1;

	println!("  br i1 %t{}, label %loop_start{}, label %loop_end{}", cond_temp, loop_start, loop_end);

	println!();
	println!("loop_start{}:", loop_start);
	*symbol_index += 1;

	generate_block(&while_.body, symbols, symbol_index);

	println!("br label %loop_cond{}", loop_cond);
	println!();
	println!("loop_end{}:", loop_end);
}

fn symtype2size(typ: SymbolType) -> usize {
	if typ == SymbolType::Bool { 1 } else { 32 }
}

fn generate_declaration(decl: &parser::DeclarationNode, parent_symbols: &Vec<&SymbolTable>, symbols: &mut SymbolTable, symbol_index: &mut usize) {

	if symbols.map.contains_key(&decl.var) {
		panic!("Generator: Declaring duplicate variable {}", str::from_utf8(&decl.var).unwrap());
	}
	
	let sym_type = if decl.typ == parser::VarType::Int {
		SymbolType::Int
	} else {
		SymbolType::Bool
	};

	let mut s = parent_symbols.clone();
	s.append(&mut vec!{&symbols});
	let expr_type = generate_expression(&decl.value, &s, symbol_index);

	if expr_type != sym_type {
		panic!("Generator: Variable declaration specifies type {:?} but is given a value of type {:?}", sym_type, expr_type);
	}

	let index = *symbol_index;

	let size = symtype2size(sym_type);

	println!("  %{}{} = alloca i{}", str::from_utf8(&decl.var).unwrap(), index, size);
	println!("  store i{} %t{}, i{}* %{}{}", size, index-1, size, str::from_utf8(&decl.var).unwrap(), index);

	*symbol_index += 1;

	symbols.map.insert(decl.var.clone(), Symbol {
		typ: sym_type,
		index,
		ret_type: None,
		arg_types: vec!{},
	});
}

fn generate_assignment(assignment: &parser::AssignmentNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) {
	let name = str::from_utf8(&assignment.var).unwrap();
	let sym = lookup_symbol(symbols, &assignment.var);

	if sym.is_none() {
		panic!("Generator: Assignment to undefined variable {}", name);
	}

	let sym = sym.unwrap();

	let value_typ = generate_expression(&assignment.value, symbols, symbol_index);

	if value_typ != sym.typ {
		panic!("Generator: Assignment to {} expected type {:?}, got type {:?}", name, sym.typ, value_typ);
	}

	let size = symtype2size(sym.typ);
	println!("  store i{} %t{}, i{}* %{}{}", size, *symbol_index-1, size, name, sym.index);
}

fn generate_return(ret: &parser::ReturnNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) {
	let expected_ret_type = lookup_symbol(symbols, &b"~return".to_vec());
	if let Some(value) = &ret.value {
		if expected_ret_type.is_none() {
			panic!("Generator: Expected no return value");
		}

		let ret_type = generate_expression(value, symbols, symbol_index);
		if ret_type != expected_ret_type.unwrap().typ {
		//if (ret_type == parser::VarType::Int && expected_ret_type.unwrap().typ != SymbolType::Int) || (ret_type == parser::VarType::Bool && expected_ret_type.unwrap().typ != SymbolType::Bool) {
			panic!("Generator: Expected return value of type {:?}, got {:?}", expected_ret_type.unwrap().typ, ret_type);
		}

		let size = symtype2size(ret_type);
		println!("  ret i{} %t{}", size, *symbol_index - 1);
	} else {
		if let Some(typ) = expected_ret_type {
			panic!("Generator: Expected return value of type {:?}", typ.typ);
		}

		println!("  ret void");
	}
}

// The temp var for this expression is always symbol_index-1 after the function call finishes
fn generate_expression(expr: &parser::ExprNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) -> SymbolType {
	let left_type = generate_val(&expr.left, symbols, symbol_index);
	let left_temp_index = *symbol_index-1;

	if let Some((op, right)) = &expr.right {
		if left_type == SymbolType::Bool {
			if op != &parser::OpType::Equals && op != &parser::OpType::NotEquals {
				panic!("Generator: Arithmatic operator {:?} used on boolean in an expression.", op);
			}
		}

		let right_type = generate_val(&right, symbols, symbol_index);
		let right_temp_index = *symbol_index-1;

		if right_type != left_type {
			panic!("Generator: Type of right side of expression does not match type of left.");
		}

		let size = symtype2size(right_type);

		match op {
			parser::OpType::Plus => println!("  %t{} = add i32 %t{}, %t{}", *symbol_index, left_temp_index, right_temp_index),
			parser::OpType::Minus => println!("  %t{} = sub i32 %t{}, %t{}", *symbol_index, left_temp_index, right_temp_index),
			parser::OpType::Multiply => println!("  %t{} = mul i32 %t{}, %t{}", *symbol_index, left_temp_index, right_temp_index),
			parser::OpType::Divide => println!("  %t{} = sdiv i32 %t{}, %t{}", *symbol_index, left_temp_index, right_temp_index),
			parser::OpType::GreaterThan => println!("  %t{} = icmp sgt i32 %t{}, %t{}", *symbol_index, left_temp_index, right_temp_index),
			parser::OpType::LessThan => println!("  %t{} = icmp slt i32 %t{}, %t{}", *symbol_index, left_temp_index, right_temp_index),
			parser::OpType::Equals => println!("  %t{} = icmp eq i{} %t{}, %t{}", *symbol_index, size, left_temp_index, right_temp_index),
			parser::OpType::NotEquals => println!("  %t{} = icmp ne i{} %t{}, %t{}", *symbol_index, size, left_temp_index, right_temp_index),
		}
		*symbol_index += 1;

		if op == &parser::OpType::GreaterThan || op == &parser::OpType::LessThan || op == &parser::OpType::Equals || op == &parser::OpType::NotEquals {
			return SymbolType::Bool;
		}
	}

	return left_type;
}

// The temp var for this expression is always symbol_index-1 after the function call finishes
fn generate_val(val: &parser::ValNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) -> SymbolType {
	if let Some(var) = &val.var {
		if let Some(sym) = lookup_symbol(symbols, &var) {
			let size = symtype2size(sym.typ);
			println!("  %t{} = load i{}, i{}* %{}{}", *symbol_index, size, size, str::from_utf8(&var).unwrap(), sym.index);
			*symbol_index += 1;
			return sym.typ;
		}
		panic!("Generator: Reference to unknown variable {}", str::from_utf8(&var).unwrap());
	} else if let Some(number) = val.number {
		// Hack, a temp var is needed by the way this compiler works but IR doesnt have a
		// way (that I can find) of just copying a constant into a var
		println!("  %t{} = add i32 {}, 0", *symbol_index, number);
		*symbol_index += 1;
		return SymbolType::Int;
	} else if let Some(bool_) = val.bool_ {
		println!("  %t{} = add i1 {}, 0", *symbol_index, if bool_ { 1 } else { 0 });
		*symbol_index += 1;
		return SymbolType::Bool;
	} else if let Some(func_call) = &val.func_call {
		let ret_type = generate_func_call(&func_call, symbols, symbol_index);
		if ret_type.is_none() {
			panic!("Generator: Function with void return value used in expression");
		}
		return ret_type.unwrap();
	} else if let Some(expr) = &val.expr {
		return generate_expression(&expr, symbols, symbol_index);
	} else {
		panic!("Generator: Invalid ValNode");
	}
}

// The return value variable for this function call is always symbol_index-1 after the function call finishes, unless the return value is void in which case no variable is defined.
fn generate_func_call(func_call: &parser::FuncCallNode, symbols: &Vec<&SymbolTable>, symbol_index: &mut usize) -> Option<SymbolType> {
	let name = str::from_utf8(&func_call.name).unwrap();
	if let Some(sym) = lookup_symbol(symbols, &func_call.name) {
		if sym.typ != SymbolType::Func {
			panic!("Generator: Call to non-function {}", name);
		}

		if sym.arg_types.len() != func_call.params.params.len() {
			panic!("Generator: Function call to {} expected {} parameters, got {}", name, sym.arg_types.len(), func_call.params.params.len());
		}

		let mut param_vars = Vec::new();

		if let Some(first_param) = func_call.params.params.first() {
			let param_type = generate_expression(first_param, symbols, symbol_index);
			if sym.arg_types[0] != param_type {
				panic!("Generator: Parameter 1 to function call {} should be type {:?}, got type {:?}", name, sym.arg_types[0], param_type);
			}
			param_vars.push((param_type, *symbol_index-1));

			let mut i = 1;
			for param in &func_call.params.params[1..] {
				let param_type = generate_expression(param, symbols, symbol_index);
				if sym.arg_types[i] != param_type {
					panic!("Generator: Parameter {} to function call {} should be type {:?}, got type {:?}", i+1, name, sym.arg_types[i], param_type);
				}
				param_vars.push((param_type, *symbol_index-1));

				i += 1;
			}
		}

		if let Some(ret_type) = sym.ret_type {
			let size = symtype2size(ret_type);
			print!("  %t{} = call i{}", *symbol_index, size);
			*symbol_index += 1;
		} else {
			print!("  call void");
		}

		print!(" @{}{}(", name, sym.index);

		let mut i = 1;
		for (param_type, idx) in &param_vars {
			let size = symtype2size(*param_type);
			print!("i{} %t{}", size, idx);
			if i != param_vars.len() {
				print!(",");
			}
			i += 1;
		}

		println!(")");
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
