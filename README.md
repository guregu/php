## php: Prolog Home Page

This is an experiment using the WebAssembly version of [Trealla Prolog](https://github.com/trealla-prolog/trealla) and [Spin](https://spin.fermyon.dev/) to host websites.
It is a retrofuturistic combination of Prolog/CGI and Rust/WebAssembly.

**Status**: Absolutely not ready for production, XSS vulnerabilities abound. Just experimenting currently.

### Setup

0. Reconsider whether you really want to do this.
1. [Install Spin](https://spin.fermyon.dev/quickstart/).
2. Clone this repo.
3. Put Prolog scripts in `cgi-bin` just like the old days.
4. Run server with `spin up`. Use `start.sh` for hot reloading.

### How does it work?

It just runs the WebAssembly version of Trealla Prolog and writes CGI ([RFC 3875](https://datatracker.ietf.org/doc/html/rfc3875)) output to stdout.

### Why?

Why not?

### Is this real?

![phpinfo/0 output](https://user-images.githubusercontent.com/131059/184548144-4903d48c-3c44-48f7-906c-d7c53d75e4c5.png)
