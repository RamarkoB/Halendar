.PHONY= update build optim clean serve serve-local

all: clean update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build 
	rm -rf public
	mkdir -p public
	cp -r static public || mkdir -p public/static
	cp cal.css public/
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin calApp | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/calApp.wasm -o public/calApp.wasm
	wasm-tools strip -o public/calApp.wasm public/calApp.wasm

serve:
	http-server public

serve-local:
	cabal run calApp

clean:
	rm -rf dist-newstyle public