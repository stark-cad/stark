# This makefile is intended to have more uses in the future

.PHONY: doc

doc:
	cargo doc --no-deps --document-private-items;
	@if [ -d "../site-src/static/doc/rust" ]; then\
	    \cp -R target/doc/* ../site-src/static/doc/rust;\
	    echo "copied rust docs into website source";\
	fi
