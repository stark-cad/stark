# SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
# SPDX-License-Identifier: AGPL-3.0-only

.PHONY: doc

doc:
	cargo doc --no-deps --document-private-items;
	@if [ -d "../site-src/static/doc/rust" ]; then\
	    \cp -R target/doc/* ../site-src/static/doc/rust;\
	    echo "copied rust docs into website source";\
	fi
