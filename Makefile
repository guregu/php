ifdef DEBUG
SPINFLAGS = WASMTIME_BACKTRACE_DETAILS=1
SPINFLAGS += RUST_LOG=spin=trace
endif

ifndef SPINCFG
SPINCFG = spin.toml
endif

ifndef SPINVER
SPINVER = v0.4.0
endif

ifndef ARCH
ARCH = amd64
endif

ifndef OS
OS = linux
endif

ifndef SPINBIN
SPINBIN = https://github.com/fermyon/spin/releases/download/$(SPINVER)/spin-$(SPINVER)-$(OS)-$(ARCH).tar.gz
endif

.PHONY: run watch deploy container

all: run
run: wasm/tpl.wasm
	sh -c '$(SPINFLAGS) spin up --file $(SPINCFG)'

watch: wasm/tpl.wasm
	nodemon --watch cgi-bin --watch www --watch lib --ext pl,html,php --verbose --legacy-watch --signal SIGINT --exec '$(SPINFLAGS) spin up --file $(SPINCFG)'

container:
	nixpacks build . --name php --pkgs wget --install-cmd 'wget -O spin.tar.gz $(SPINBIN) && tar xvf spin.tar.gz' --start-cmd './spin up --file spin.toml'

deploy: wasm/tpl.wasm
	ssh ubuntu@php.energy 'cd php && git pull && sudo systemctl restart php'
