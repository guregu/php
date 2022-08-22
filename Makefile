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

.PHONY: run watch container deploy

all: run

build:
	spin build --file $(SPINCFG)

run: build
	$(SPINFLAGS) spin up --file $(SPINCFG)

watch: build
	nodemon --watch cgi-bin --watch www --watch lib --ext pl,html,php --verbose --legacy-watch --signal SIGINT --exec '$(SPINFLAGS) spin up --file $(SPINCFG)'

container: build
	nixpacks build . --name php --pkgs wget curl \
		--install-cmd 'wget -O spin.tar.gz https://github.com/fermyon/spin/releases/download/v0.4.0/spin-v0.4.0-linux-amd64.tar.gz && tar xvf spin.tar.gz && (curl https://get.wasmer.io -sSfL | sh)' \
		--build-cmd './spin build' \
		--start-cmd './spin up --file spin.toml'

deploy:
	ssh ubuntu@php.energy 'cd php && git pull && sudo systemctl restart php'
