BABBELAAR=target/debug/babbelaar-interpreter

bouw:
	cargo build --all
	$(BABBELAAR) bouwen babbib
