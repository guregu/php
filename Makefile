ifdef DEBUG
SPINFLAGS = WASMTIME_BACKTRACE_DETAILS=1
SPINFLAGS += RUST_LOG=spin=trace
endif

ifndef SPINCFG
SPINCFG = spin.toml
endif

ifndef SPINVER
SPINVER = v2.7.0
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
	$(SPINFLAGS) spin up -f $(SPINCFG)

watch: build
	spin watch --skip-build --direct-mounts

# standalone container
container: build
	nixpacks build . --name php --pkgs wget \
		--install-cmd 'wget -O spin.tar.gz $(SPINBIN) && tar xvf spin.tar.gz' \
		--build-cmd './spin build' \
		--start-cmd './spin up --file spin.toml --listen 0.0.0.0:3000'
