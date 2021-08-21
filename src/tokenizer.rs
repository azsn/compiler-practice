use std::str;
use regex::bytes::Regex;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
	Func, // func
	Int, // int
	Bool, // bool
	True, // true
	False, // false
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
	static ref TOKEN_REGEX: [(TokenType, Regex); 26] = [
		(TokenType::Func,       Regex::new(r"\A\bfunc\b").unwrap()),
		(TokenType::Int,        Regex::new(r"\A\bint\b").unwrap()),
		(TokenType::Bool,       Regex::new(r"\A\bbool\b").unwrap()),
		(TokenType::True,       Regex::new(r"\A\btrue\b").unwrap()),
		(TokenType::False,      Regex::new(r"\A\bfalse\b").unwrap()),
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

#[derive(Debug)]
pub struct Token {
	typ: TokenType,
	value: Vec<u8>, // Only filled on Identifier or Number
}

pub fn tokenize(ccode: &[u8]) -> Vec<Token> {
	let mut code = ccode;
	let mut tokens = Vec::new();

	'outer: while code.len() > 0 {
		// Trim leading whitespace
		while code[0] == b' ' || code[0] == b'\n' || code[0] == b'\r' || code[0] == b'\t' {
			code = &code[1..]
		}

		for (typ, regex) in TOKEN_REGEX.iter() {
			if let Some(mat) = regex.find(&code) {
				let typ = *typ;
				let value = if typ == TokenType::Identifier
					|| typ == TokenType::Number {
					code[..mat.end()].to_vec()
				} else {
					b"".to_vec()
				};

				tokens.push(Token{ typ, value });
				code = &code[mat.end()..];
				continue 'outer;
			}
		}

		panic!("Tokenize matched no token types: {:?}", str::from_utf8(code).unwrap());
	}

	return tokens;
}
