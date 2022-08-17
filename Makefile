ifdef DEBUG
SPINFLAGS = WASMTIME_BACKTRACE_DETAILS=1
SPINFLAGS += RUST_LOG=spin=trace
endif

ifndef SPINCFG
SPINCFG = spin.toml
endif

all: local

local: wasm/tpl.wasm
	sh -c '$(SPINFLAGS) spin up --file $(SPINCFG)'

watch: wasm/tpl.wasm
	nodemon --watch cgi-bin --watch www --watch lib --ext pl,html,php --verbose --legacy-watch --signal SIGINT --exec '$(SPINFLAGS) spin up --file $(SPINCFG)'

deploy: wasm/tpl.wasm
	ssh ubuntu@php.energy 'cd php && git pull && sudo systemctl restart php'