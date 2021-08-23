use super::tokenizer::Token;
use super::tokenizer::TokenType;
use std::str;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VarType {
	Int,
	Bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OpType {
	Plus,
	Minus,
	Multiply,
	Divide,
	GreaterThan,
	LessThan,
	Equals,
	NotEquals,
}

#[derive(Debug, Clone)]
pub struct ProgramNode {
	pub functions: Vec<FuncNode>,
}

#[derive(Debug, Clone)]
pub struct FuncNode {
	pub name: Vec<u8>,
	pub args: ArgsNode,
	pub return_type: Option<VarType>,
	pub body: BlockNode,
}

#[derive(Debug, Clone)]
pub struct ArgsNode {
	pub args: Vec<(VarType, Vec<u8>)>, // (arg type, arg name)
}

#[derive(Debug, Clone)]
pub struct BlockNode {
	pub statements: Vec<StatementNode>
}

#[derive(Debug, Clone, Default)]
pub struct StatementNode {
	// Only one of these will be set
	pub if_: Option<IfNode>,
	pub while_: Option<WhileNode>,
	pub declaration: Option<DeclarationNode>,
	pub assignment: Option<AssignmentNode>,
	pub func_call: Option<FuncCallNode>,
	pub return_: Option<ReturnNode>,
}

#[derive(Debug, Clone)]
pub struct IfNode {
	pub condition: ExprNode,
	pub true_body: BlockNode,
	pub false_body: Option<BlockNode>,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
	pub condition: ExprNode,
	pub body: BlockNode,
}

#[derive(Debug, Clone)]
pub struct DeclarationNode {
	pub typ: VarType,
	pub var: Vec<u8>,
	pub value: ExprNode,
}

#[derive(Debug, Clone)]
pub struct AssignmentNode {
	pub var: Vec<u8>,
	pub value: ExprNode,
}

#[derive(Debug, Clone)]
pub struct FuncCallNode {
	pub name: Vec<u8>,
	pub params: ParamsNode,
}

#[derive(Debug, Clone)]
pub struct ParamsNode {
	pub params: Vec<ExprNode>
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
	pub value: Option<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct ExprNode {
	pub left: ValNode,
	pub right: Option<(OpType, ValNode)>,
}

#[derive(Debug, Clone, Default)]
pub struct ValNode {
	// Only one of these will be set
	pub var: Option<Vec<u8>>,
	pub number: Option<i64>,
	pub bool_: Option<bool>,
	pub func_call: Option<FuncCallNode>,
	pub expr: Option<Box<ExprNode>>,
}

pub struct Parser {
	tokens: Vec<Token>
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Parser {
		Parser { tokens }
	}

	fn consume(&mut self, expect: TokenType) -> Token {
		if let Some(first) = self.tokens.first() {
			if first.typ == expect {
				return self.tokens.remove(0);
			}
			panic!("Parser: Expected {:?}, found {:?}", expect, first);
		} else {
			panic!("Parser: Expected {:?}, found EOF", expect);
		}
	}

	fn peek(&self, expect: TokenType, ahead: usize) -> bool {
		if let Some(at) = self.tokens.get(ahead) {
			return at.typ == expect;
		}
		return false;
	}

	pub fn parse(&mut self) -> ProgramNode {
		let mut n = ProgramNode {
			functions: Vec::new(),
		};
		while self.tokens.len() != 0 {
			n.functions.push(self.parse_func());
		}
		return n;
	}

	fn parse_func(&mut self) -> FuncNode {
		self.consume(TokenType::Func);
		let name_token = self.consume(TokenType::Identifier);

		self.consume(TokenType::OParen);

		let args = self.parse_args();

		self.consume(TokenType::CParen);

		let return_type = if !self.peek(TokenType::OBrace, 0) {
			Some(self.parse_type())
		} else {
			None
		};

		let body = self.parse_block();

		return FuncNode {
			name: name_token.value,
			args,
			return_type,
			body,
		};
	}

	fn parse_args(&mut self) -> ArgsNode {
		let mut n = ArgsNode { args: Vec::new() };
		if !self.peek(TokenType::CParen, 0) {
			n.args.push(self.parse_arg());
			while self.peek(TokenType::Comma, 0) {
				self.consume(TokenType::Comma);
				n.args.push(self.parse_arg());
			}
		}
		return n;
	}

	fn parse_arg(&mut self) -> (VarType, Vec<u8>) {
		let typ = self.parse_type();
		let name_token = self.consume(TokenType::Identifier);
		return (typ, name_token.value);
	}

	fn parse_type(&mut self) -> VarType {
		let type_token = self.consume(TokenType::Type);
		match type_token.value.as_slice() {
			b"int" => { return VarType::Int; }
			b"bool" => { return VarType::Bool; }
			// Shouldn't happen if tokenizer works
			_ => { panic!("Invalid type token {:?}", type_token); }
		}

	}

	fn parse_block(&mut self) -> BlockNode {
		self.consume(TokenType::OBrace);

		let mut s = Vec::new();

		while !self.peek(TokenType::CBrace, 0) {
			s.push(self.parse_statement());
		}

		self.consume(TokenType::CBrace);

		return BlockNode {
			statements: s
		};
	}

	fn parse_statement(&mut self) -> StatementNode {
		let mut n = StatementNode { ..Default::default() };

		if self.peek(TokenType::If, 0) {
			n.if_ = Some(self.parse_if());
		} else if self.peek(TokenType::While, 0) {
			n.while_ = Some(self.parse_while());
		} else if self.peek(TokenType::Type, 0) {
			n.declaration = Some(self.parse_declaration());
		} else if self.peek(TokenType::Identifier, 0) {
			if self.peek(TokenType::OParen, 1) {
				n.func_call = Some(self.parse_func_call());
				self.consume(TokenType::Semicolon);
			} else {
				n.assignment = Some(self.parse_assignment());
			}
		} else if self.peek(TokenType::Return, 0) {
			n.return_ = Some(self.parse_return());
		} else {
			panic!("Parser: Invalid statement beginning: {:?}", self.tokens.first());
		}

		return n;
	}

	fn parse_if(&mut self) -> IfNode {
		self.consume(TokenType::If);
		self.consume(TokenType::OParen);
		let condition = self.parse_expr();
		self.consume(TokenType::CParen);
		let true_body = self.parse_block();

		let mut false_body = None;
		if self.peek(TokenType::Else, 0) {
			self.consume(TokenType::Else);
			false_body = Some(self.parse_block());
		}

		return IfNode { condition, true_body, false_body };
	}

	fn parse_while(&mut self) -> WhileNode {
		self.consume(TokenType::While);
		self.consume(TokenType::OParen);
		let condition = self.parse_expr();
		self.consume(TokenType::CParen);
		let body = self.parse_block();
		return WhileNode { condition, body };
	}

	fn parse_declaration(&mut self) -> DeclarationNode {
		let typ = self.parse_type();
		let var = self.consume(TokenType::Identifier).value;
		self.consume(TokenType::Assignment);
		let value = self.parse_expr();
		self.consume(TokenType::Semicolon);
		return DeclarationNode { typ, var, value };
	}

	fn parse_assignment(&mut self) -> AssignmentNode {
		let var = self.consume(TokenType::Identifier).value;
		self.consume(TokenType::Assignment);
		let value = self.parse_expr();
		self.consume(TokenType::Semicolon);
		return AssignmentNode { var, value };
	}

	fn parse_func_call(&mut self) -> FuncCallNode {
		let name = self.consume(TokenType::Identifier).value;
		self.consume(TokenType::OParen);
		let params = self.parse_params();
		self.consume(TokenType::CParen);
		return FuncCallNode { name, params };
	}

	fn parse_params(&mut self) -> ParamsNode {
		let mut n = ParamsNode { params: Vec::new() };
		if !self.peek(TokenType::CParen, 0) {
			n.params.push(self.parse_expr());
			while self.peek(TokenType::Comma, 0) {
				self.consume(TokenType::Comma);
				n.params.push(self.parse_expr());
			}
		}
		return n;
	}

	fn parse_return(&mut self) -> ReturnNode {
		self.consume(TokenType::Return);
		let value = if self.peek(TokenType::Semicolon, 0) {
			None
		} else {
			Some(self.parse_expr())
		};
		self.consume(TokenType::Semicolon);
		return ReturnNode { value };
	}

	fn parse_expr(&mut self) -> ExprNode {
		let left = self.parse_val();

		let right;
		if self.peek(TokenType::Plus, 0) {
			self.consume(TokenType::Plus);
			right = Some((OpType::Plus, self.parse_val()));
		} else if self.peek(TokenType::Minus, 0) {
			self.consume(TokenType::Minus);
			right = Some((OpType::Minus, self.parse_val()));
		} else if self.peek(TokenType::Multiply, 0) {
			self.consume(TokenType::Multiply);
			right = Some((OpType::Multiply, self.parse_val()));
		} else if self.peek(TokenType::Divide, 0) {
			self.consume(TokenType::Divide);
			right = Some((OpType::Divide, self.parse_val()));
		} else if self.peek(TokenType::GreaterThan, 0) {
			self.consume(TokenType::GreaterThan);
			right = Some((OpType::GreaterThan, self.parse_val()));
		} else if self.peek(TokenType::LessThan, 0) {
			self.consume(TokenType::LessThan);
			right = Some((OpType::LessThan, self.parse_val()));
		} else if self.peek(TokenType::Equals, 0) {
			self.consume(TokenType::Equals);
			right = Some((OpType::Equals, self.parse_val()));
		} else if self.peek(TokenType::NotEquals, 0) {
			self.consume(TokenType::NotEquals);
			right = Some((OpType::NotEquals, self.parse_val()));
		} else {
			right = None;
		}

		return ExprNode { left, right };
	}

	fn parse_val(&mut self) -> ValNode {
		let mut n = ValNode { ..Default::default() };

		if self.peek(TokenType::Identifier, 0) {
			if self.peek(TokenType::OParen, 1) {
				n.func_call = Some(self.parse_func_call());
			} else {
				n.var = Some(self.consume(TokenType::Identifier).value);
			}
		} else if self.peek(TokenType::Number, 0) {
			let num_val = self.consume(TokenType::Number).value;
			let num_str = str::from_utf8(&num_val).unwrap();
			let num = num_str.parse::<i64>().unwrap();
			n.number = Some(num);
		} else if self.peek(TokenType::Bool, 0) {
			let bool_val = self.consume(TokenType::Bool).value;
			let bool_ = bool_val[0] == b't';
			n.bool_ = Some(bool_);
		} else if self.peek(TokenType::OParen, 0) {
			self.consume(TokenType::OParen);
			n.expr = Some(Box::new(self.parse_expr()));
			self.consume(TokenType::CParen);
		} else {
			panic!("Parser: Invalid token in expression: {:?}", self.tokens.first());
		}

		return n;
	}
}

//while !self.peek(TokenType::Semicolon, 0) && !self.peek(TokenType::CParen, 0) {
//	self.tokens.remove(0);
//}

//pub fn new(tokens: &[Token]) -> Parser {
//	Parser { tokens.to_vec() }
//}

//pub fn parse(tokens: &[Token]) -> ParseNode {
//	return ParseNode {};
//}

//pub fn parse_program(tokens: &[Token]) -> ParseNode {
//	p
//}

//fn consume<'a>(t: &'a mut &[Token], expect: TokenType) -> &'a Token {
//	if let Some(first) = t.first() {
//		if first.typ == expect {
//			return first;
//		}
//		panic!("Parser: Expected {:?}, found {:?}", expect, first);
//	} else {
//		panic!("Parser: Expected {:?}, found EOF", expect);
//	}
//}

//fn mutvar(v: &mut &[i32]) {
//	*v = &v[1..];
//}

#[test]
fn test_parser_new() {

	let tokens: [Token; 2] = [
		Token{ typ: TokenType::Func, value: vec!{} },
		Token{ typ: TokenType::Identifier, value: vec!{97, 98, 99} },
	];

	let p = Parser::new(tokens.to_vec());
	let f = p.consume(TokenType::Func);

	//let mut stokens = &tokens[..];
	//let t1 = consume(&mut stokens, TokenType::Func);
	//println!("found: {:?}", t1);
	//let t2 = consume(&mut stokens, TokenType::Identifier);
	//println!("found: {:?}", t2);

	//let a: [i32; 5] = [9, 7, 5, 3, 2];
	//let mut b = &a[..];
	//b = &b[1..];
	//mutvar(&mut b);
	//let t = a.first();
	//println!("{:?}", a);
	//println!("{:?}", b);
	//println!("{:?}", t);
	assert_eq!(false, true);
}
