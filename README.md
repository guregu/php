# php: Prolog Home Page

This is an experiment using the WebAssembly version of [Trealla Prolog](https://github.com/trealla-prolog/trealla) and [Spin](https://spin.fermyon.dev/) to host websites.
It is a retrofuturistic combination of Prolog/C and Rust/WebAssembly.

**Status**: Slowly approaching stability? Still in cowboy mode. 🤠

## Demo

Head on over to our beautiful homepage at [php.energy](https://php.energy/). Source code examples included.

## Setup

0. ~~Reconsider whether you really want to do this~~.
1. [Install Spin](https://spin.fermyon.dev/quickstart/).
2. For hot reloading, [install nodemon](https://www.npmjs.com/package/nodemon) (optional).
3. Clone this repo.
4. Configure the www root in `spin.toml`
5. Put PHP scripts or Prolog programs in `public_html`.

## Run

- Run server with `make` or `spin up`.
- Or use `make watch` for hot reloading.

## Deploy to \~the cloud\~ 🆕

As of recently, you can deploy to the beta version of Fermyon cloud for free. Neat. [More info here](https://www.fermyon.com/blog/introducing-fermyon-cloud).

1. [Get set up](https://developer.fermyon.com/cloud/deploy)
2. `spin deploy`

## Container Build

See: [Spin docs on OCI images](https://developer.fermyon.com/spin/spin-oci)

## How does it work?

It just runs the WebAssembly version of Trealla Prolog and writes CGI ([RFC 3875](https://datatracker.ietf.org/doc/html/rfc3875)) output to stdout.

See the README in the www folder for more info on the file structure.

Currently this uses a fork of Trealla hosted on WAPM, but it works with upstream Trealla too.

## What's next?

This currently uses the CGI mode of Spin, but with a little bit of effort we could use the fancy APIs and get outgoing HTTP and Redis and whatnot.
It'd be cool to get some kind of magic persistence going.

## File Layout

### public_html

Put PHP templates here and they will show up in your root.

## Templates

Files ending in `.html` will be interpreted as PHP templates with the following special syntax.

### Queries: `<?- Goal. ?>`
```prolog
<?- current_prolog_flag(version, Ver), write(Ver) ?>
<?php ... ?>
```

You can use the `<?- Goal. ?>` (alias: `<?php Goal. ?>`) expression to execute Prolog (of course). 

By default, all output from these blocks will be escaped to avoid XSS attacks. You can also use unsafe queries, see below.

#### Unsafe queries: `<?unsafe Goal. ?>`

You can use the `<?unsafe Goal. ?>` expression to execute Prolog code without escaping its output (dangerous!).
Avoid using this if you can.

For example, this renders a table of the numbers 1-10 and their squares:

```html
<h3>Math</h3>
<table>
	<tr><th>N</th><th>N²</th></tr>
	<?unsafe
		bagof([N, Square], (between(1, 10, N), Square is N^2)), Flags),
		maplist(format("<tr><td>~w</td><td>~w</td></tr>"), Flags)
	?>
</table>
```

#### if block: `<?if Goal. ?> ... <?end ?>`
```html
<?if current_prolog_flag(dialect, X). ?>
	You are using: <?=X ?>
<?end ?>
```

Conditionally executes the block if Goal succeeds. Only runs once.

#### findall block: `<?findall Goal. ?> ... <?end ?>`
```html
<table>
<?findall current_prolog_flag(Key, Value). ?>
	<tr><td><?=Key ?></td><td><?=Value ?></td></tr>
<?end ?>
</table>
```

Works like if blocks, but with findall behavior.

#### findall query: `<?* Goal. ?>`
```prolog
<?* member(X, [1, 2, 3]), write(X). ?>
```

Works the same as query, but findall behavior instead of once behavior.

#### Echo: `<?=Var Goal. ?>`
```html
1+1 = <?=X X is 1+1. ?>
<input type="text" name="ask" value="<?=Param query_param(ask, Param) ?>">
```

Works the same as query, but echoes (writes) the variable bound to Var. Escapes output.

### Assertions: `<? Program ?>`
```prolog
<?
% declare rules and facts
best_web_framework(php).
good_enough(X) :- between(1, 640, X).

% "directives" get executed upon evaluation
:- echo("hello!").
:- succ(68, X), write(X).
?>

The web framework of the future is <?=Framework best_web_framework(Framework). ?>
```

Assert facts and rules as if consulting a Prolog program. Directive syntax will call the given goal.

`<?prolog ... ?>` is an alias for this.

## API

### module(cgi)

#### env/2

Envrionment variables.

#### query_param/2

Query parameters (from URL).

### module(php)

#### php//1

PHP grammar.

#### phpinfo/0
```prolog
phpinfo.
```

Writes HTML output, dumping the current environment and flags.

#### render/1
```prolog
render(+Filename).
```

Renders the given PHP file.

#### html_escape/2
```prolog
html_escape(-Raw:string, +Sanitized:string).
```

Escapes string using `htmlspecialentities//1`.

## FAQ

### Why?

Why not?

### Is this real?

![phpinfo/0 output](https://user-images.githubusercontent.com/131059/184548289-46cca2e2-8bfe-4684-b96a-8f4311f03a4a.png)
