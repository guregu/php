#!/bin/sh
nodemon --watch cgi-bin --watch std --ext pl --verbose --legacy-watch --signal SIGINT --exec 'WASMTIME_BACKTRACE_DETAILS=1 spin up --file spin.toml'
