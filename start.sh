#!/bin/sh
nodemon --watch cgi-bin --watch www --watch lib --ext pl,html,php --verbose --legacy-watch --signal SIGINT --exec 'WASMTIME_BACKTRACE_DETAILS=1 spin up --file spin.toml'
