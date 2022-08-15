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

#### cgi-bin

Runs the `main/0` predicate from a `.pl` file matching the file path.

#### public_html

You can use the `<?php goal ?>` expression to execute Prolog (of course). See the README in the www folder for more info on the file structure.

For example, this renders a table of the current Prolog flags:

```html
<h3>Prolog flags</h3>
<table>
	<tr><th>Flag</th><th>Value</th></tr>
	<?php
		bagof([K, V], current_prolog_flag(K, V), Flags),
		maplist(format("<tr><td>~w</td><td>~w</td></tr>"), Flags)
	?>
</table>
```

### Why?

Why not?

### Is this real?

![phpinfo/0 output](https://user-images.githubusercontent.com/131059/184548289-46cca2e2-8bfe-4684-b96a-8f4311f03a4a.png)
