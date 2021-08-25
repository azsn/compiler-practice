#[macro_use]
extern crate lazy_static;

mod tokenizer;
mod parser;
mod generator;

fn main() {
	let tokens = tokenizer::tokenize(r"
	func g() bool {
		return ((true == false) == false);
	}
	func f(int x, bool y) int {
		bool a = g() != y;
		print_bool(a);
		if (y) {
			int z = x * 2;
			z = z + 1;
			return (3 + (z * 7)) / 2;
		} else {
			return x;
		}
	}
	func recurse(int x) int {
		if (x == 0) {
			return 0;
		}

		int s = recurse(x - 1);
		print_int(x);
		return s + x;
	}
	func main() int {
		print_int(f(4, false));
		print_int(f(9, true) - 5);
		int i = 10;
		while (i > 0) {
			print_int(i);
			i = i - 1;
		}

		print_bool(false);
		print_int(recurse(5));
		return 0;
	}".as_bytes());
	let mut p = parser::Parser::new(tokens);
	let ast = p.parse();
	//println!("{:?}", ast);
	generator::generate(&ast);
}

