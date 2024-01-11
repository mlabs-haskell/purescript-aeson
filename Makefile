SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.PHONY: build test format check-format

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
js-sources := $(shell fd --no-ignore-parent -ejs -ecjs)

build:
	spago build

test:
	spago test

clean:
	rm -f bundle.js

.ONESHELL:
check-explicit-exports:
	@if grep -rn '(\.\.)' ${ps-sources}; then
		echo "Use explicit imports/exports ^"
		exit 1
	else
		echo "All imports/exports are explicit"
	fi

check-format: clean check-explicit-exports
	@purs-tidy check ${ps-sources}
	@nixpkgs-fmt --check ${nix-sources}
	@prettier --log-level warn -c ${js-sources}
	@eslint --quiet ${js-sources} --parser-options 'sourceType: module'

format: clean
	@purs-tidy format-in-place ${ps-sources}
	@nixpkgs-fmt ${nix-sources}
	@prettier -w ${js-sources}
	@make check-format

# Run Nix CI locally
run-ci-actions:
	nix build -L .#checks.x86_64-linux.formatting-check
	nix build -L .#checks.x86_64-linux.tests
