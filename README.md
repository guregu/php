# php: Prolog Home Page

This is an experiment using the WebAssembly version of [Trealla Prolog](https://github.com/trealla-prolog/trealla) and [Spin](https://spin.fermyon.dev/) to host websites.
It is a retrofuturistic combination of Prolog/C/CGI and Rust/WebAssembly.

**Status**: Slowly approaching stability? Still in cowboy mode. 🤠

## Demo

Head on over to our beautiful homepage at [php.energy](https://php.energy/). Source code examples included.

## Setup

0. ~~Reconsider whether you really want to do this~~.
1. [Install Spin](https://spin.fermyon.dev/quickstart/).
2. [Install WAPM](https://docs.wasmer.io/ecosystem/wasmer/getting-started)
3. For hot reloading, [install nodemon](https://www.npmjs.com/package/nodemon) (optional).
4. For building the container image, [install nixpacks](https://nixpacks.com/docs/getting-started).
5. Clone this repo.
6. Configure the www root in `spin.toml`
7. Put PHP scripts or Prolog programs in `public_html` or `cgi-bin`.

## Run

- Run server with `make` or `spin up`.
- Or use `make watch` for hot reloading.

## Deploy to \~the cloud\~ 🆕

As of recently, you can deploy to the beta version of Fermyon cloud for free. Neat. [More info here](https://www.fermyon.com/blog/introducing-fermyon-cloud).

1. [Get set up](https://developer.fermyon.com/cloud/deploy)
2. `spin build`
3. `spin deploy`

## Container Build

1. [Install nixpacks](https://nixpacks.com/docs/getting-started).
2. Build a Docker container image with `make container`.
3. See Makefile for settings.

## How does it work?

It just runs the WebAssembly version of Trealla Prolog and writes CGI ([RFC 3875](https://datatracker.ietf.org/doc/html/rfc3875)) output to stdout.

See the README in the www folder for more info on the file structure.

Currently this uses a fork of Trealla hosted on WAPM, but it works with upstream Trealla too.

## What's next?

This currently uses the CGI mode of Spin, but with a little bit of effort we could use the fancy APIs and get outgoing HTTP and Redis and whatnot.
It'd be cool to get some kind of magic persistence going.

## Library

### cgi-bin

Runs the `main/0` predicate from a `.pl` file matching the file path.

### public_html

Put PHP templates here and they will show up in your root.

## Templates

Files ending in `.html` will be interpreted as PHP templates with the following special syntax.

### Queries: `<?- Goal ?>`
```prolog
<?- current_prolog_flag(version, Ver), write(Ver) ?>
<?php ... ?>
```

You can use the `<?php goal ?>` expression to execute Prolog (of course). 

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

#### findall: `<?* Goal ?>`
```prolog
<?* member(X, [1, 2, 3]) ?>
```
Works the same as query, but findall behavior instead of once behavior.

#### Echo: `<?=Var Goal ?>`
```html
1+1 = <?=X X is 1+1 ?>
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
:- echo "hello!".
:- succ(68, X), write(X).
?>
<?prolog ... ?>
```

Assert facts and rules as if consulting a Prolog program. Directive syntax will call the given goal.

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
