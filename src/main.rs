#[macro_use]
extern crate lazy_static;

mod tokenizer;
mod parser;

fn main() {
	let tokens = tokenizer::tokenize(r"func f(int x, bool y) int {
		if (y) {
			int z = x * 2;
			return (3 == (z * 3)) / 9;
		} else { return 1; }
		return x;
	}
	func main() {
		print(f(5, false));
		print(f(9, true) - 5);
		return;
	}".as_bytes());
	let mut p = parser::Parser::new(tokens);
	let ast = p.parse();
	println!("{:?}", ast);
}

