run:
	cargo run > temp.ll
	clang temp.ll
	./a.out
#cargo run | clang -x ll - && ./a.out
