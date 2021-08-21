#[macro_use]
extern crate lazy_static;

mod tokenizer;

fn main() {
	let tokens = tokenizer::tokenize(r"func f(int x, bool y) int {
		if (y) {
			int z = x * 2;
			return z * 3;
		}
		return x;
	}".as_bytes());
	println!("{:?}", tokens);
}

