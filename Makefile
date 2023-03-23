ifdef DEBUG
SPINFLAGS = WASMTIME_BACKTRACE_DETAILS=1
SPINFLAGS += RUST_LOG=spin=trace
endif

ifndef SPINCFG
SPINCFG = spin.toml
endif

ifndef SPINVER
SPINVER = v0.8.0
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

.PHONY: run watch container

all: run

build:
	spin build --from $(SPINCFG)

run: build
	$(SPINFLAGS) spin up --from $(SPINCFG) --follow-all

watch: build
	nodemon --watch cgi-bin --watch . --ext pl,html,wasm --verbose --legacy-watch --signal SIGINT --exec '$(SPINFLAGS) spin up --file $(SPINCFG)'

container: build
	nixpacks build . --name php --pkgs wget curl \
		--install-cmd 'wget -O spin.tar.gz $(SPINBIN) && tar xvf spin.tar.gz && (curl https://get.wasmer.io -sSfL | sh)' \
		--build-cmd './spin build' \
		--start-cmd './spin up --file spin.toml'

oci:
	$(SPINFLAGS) spin oci push ghcr.io/guregu/php:v0.0.1-test1