#[macro_use]
extern crate lazy_static;

mod tokenizer;
mod parser;
mod generator;

fn main() {
	let tokens = tokenizer::tokenize(r"func f(int x, bool y) int {
		bool a = true != false;
		if (y) {
			int z = x * 2;
			return (3 + (z * 3)) / 2;
		} else { return 1; }
		return x;
	}
	func main() int {
		print_int(f(5, false));
		print_int(f(9, true) - 5);
		return 0;
	}".as_bytes());
	let mut p = parser::Parser::new(tokens);
	let ast = p.parse();
	//println!("{:?}", ast);
	generator::generate(&ast);
}

