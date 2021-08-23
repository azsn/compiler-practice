use std::str;
use regex::bytes::Regex;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
	Func, // func
	Type, // (int|bool)
	Bool, // (true|false)
	If, // if
	Else, // else
	While, // while
	Return, // return
	Equals, // ==
	NotEquals, // !=
	OParen, // (
	CParen, // )
	OBrace, // {
	CBrace, // }
	Comma, // ,
	Semicolon, // ;
	Assignment, // =
	Plus, // +
	Minus, // -
	Multiply, // *
	Divide, // /
	GreaterThan, // >
	LessThan, // <
	Identifier, // [a-zA-Z_]+
	Number, // [0-9]+
}

lazy_static! {
	static ref TOKEN_REGEX: [(TokenType, Regex); 24] = [
		(TokenType::Func,       Regex::new(r"\A\bfunc\b").unwrap()),
		(TokenType::Type,        Regex::new(r"\A\b(int|bool)\b").unwrap()),
		(TokenType::Bool,       Regex::new(r"\A\b(true|false)\b").unwrap()),
		(TokenType::If,         Regex::new(r"\A\bif\b").unwrap()),
		(TokenType::Else,       Regex::new(r"\A\belse\b").unwrap()),
		(TokenType::While,      Regex::new(r"\A\bwhile\b").unwrap()),
		(TokenType::Return,     Regex::new(r"\A\breturn\b").unwrap()),
		(TokenType::Equals,     Regex::new(r"\A==").unwrap()),
		(TokenType::NotEquals,  Regex::new(r"\A!=").unwrap()),
		(TokenType::OParen,     Regex::new(r"\A\(").unwrap()),
		(TokenType::CParen,     Regex::new(r"\A\)").unwrap()),
		(TokenType::OBrace,     Regex::new(r"\A\{").unwrap()),
		(TokenType::CBrace,     Regex::new(r"\A\}").unwrap()),
		(TokenType::Comma,      Regex::new(r"\A,").unwrap()),
		(TokenType::Semicolon,  Regex::new(r"\A;").unwrap()),
		(TokenType::Assignment, Regex::new(r"\A=").unwrap()),
		(TokenType::Plus,       Regex::new(r"\A\+").unwrap()),
		(TokenType::Minus,      Regex::new(r"\A-").unwrap()),
		(TokenType::Multiply,   Regex::new(r"\A\*").unwrap()),
		(TokenType::Divide,     Regex::new(r"\A/").unwrap()),
		(TokenType::GreaterThan,Regex::new(r"\A>").unwrap()),
		(TokenType::LessThan,   Regex::new(r"\A<").unwrap()),
		(TokenType::Identifier, Regex::new(r"\A[a-zA-Z_]+").unwrap()),
		(TokenType::Number,     Regex::new(r"\A[0-9]+").unwrap()),
	];
}

#[derive(Debug, Clone)]
pub struct Token {
	pub typ: TokenType,
	pub value: Vec<u8>, // Only filled on Identifier or Number
	pub line: usize,
	pub col: usize,
}

pub fn tokenize(ccode: &[u8]) -> Vec<Token> {
	let mut code = ccode;
	let mut tokens = Vec::new();
	let mut line: usize = 1;
	let mut col: usize = 1;

	'outer: while code.len() > 0 {
		// Trim leading whitespace
		while code[0] == b' ' || code[0] == b'\n' || code[0] == b'\r' || code[0] == b'\t' {
			if code[0] == b'\n' || code[0] == b'\r' {
				line += 1;
				col = 0;
			}
			code = &code[1..];
			col += 1;
		}

		for (typ, regex) in TOKEN_REGEX.iter() {
			if let Some(mat) = regex.find(&code) {
				let typ = *typ;
				let value = if typ == TokenType::Identifier
					|| typ == TokenType::Number 
					|| typ == TokenType::Bool
					|| typ == TokenType::Type {
					code[..mat.end()].to_vec()
				} else {
					Vec::new()
				};

				tokens.push(Token{ typ, value, line, col });
				code = &code[mat.end()..];
				col += mat.end();
				continue 'outer;
			}
		}

		panic!("Tokenizer: Code does not match any token type: {:?}", str::from_utf8(code).unwrap());
	}

	return tokens;
}
