# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.


NIXPKGS := $(shell nix-build -Q --no-out-link ./nix/fetch-nixpkgs.nix 2>/dev/null)

nix-build-attr = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS) -A $(1)

nix-build = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS)

pinpon:	nix
	nix-build --no-out-link nix/jobsets/testing.nix -I nixpkgs=$(NIXPKGS) -A pinpon

nixpkgs:	nix
		$(call nix-build-attr,nixpkgs)

lts-%:	nix
	$(call nix-build-attr,lts-$*)

release: nix
	 $(call nix-build)

doc:	test
	@echo "*** Generating docs"
	cabal haddock --hyperlink-source

test:	build
	@echo "*** Running tests"
	cabal test

help:
	@echo "Targets:"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "(Default is 'pinpon')"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    pinpon    - build pinpon against nixpkgs using nix-build (quick)"
	@echo "    nixpkgs   - build pinpon against nixpkgs using nix-build"
	@echo "    lts-10    - build pinpon against LTS 10 package set using nix-build"
	@echo "    lts-9     - build pinpon against LTS 9 package set using nix-build"
	@echo "    release   - Run nix-build on all release.nix targets"
	@echo
	@echo "    test      - configure and build the package, then run the tests"
	@echo "    build     - configure and build the package"
	@echo "    configure - configure the package"
	@echo
	@echo "Stack/Nix:"
	@echo
	@echo "The following targets build and test the package with Stack, using the"
	@echo "given version of Stackage LTS as configured by the file stack-<target>.yaml."
	@echo
	@echo "    stack-lts    [build all supported LTS targets]"
	@echo "    stack-lts-10"
	@echo "    stack-lts-9"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	configure
	@echo "*** Building the package"
	cabal build

sdist:	check doc
	@echo "*** Creating a source distribution"
	cabal sdist

check:
	@echo "*** Checking the package for errors"
	cabal check

configure: nix pinpon.cabal
	@echo "*** Configuring the package"
	cabal configure -f test-hlint

nix: 	pinpon.cabal
	@echo "*** Generating pkgs/pinpon.nix"
	cd nix/pkgs && cabal2nix ../../. > pinpon.nix
	cd nix/pkgs && cabal2nix --flag test-hlint ../../. > pinpon-hlint.nix

pinpon.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

nix-stack = nix-shell -p stack-env zlib libiconv ncurses --run 'stack test --stack-yaml $(1)'

stack-lts:      stack-lts-10 stack-lts-9

stack-lts-%:    nix
		$(call nix-stack, stack-lts-$*.yaml)

.PHONY: clean nix
