:- use_module(library(spin)).

http_handler(get("/", _), _, _, 200) :-
	html_content,
	% trace,
	setup_call_cleanup(
		store_open(default, Store),
		(
			(  store_get(Store, counter, N0)
			-> true
			;  N0 = 0
			),
			succ(N0, N),
			store_set(Store, counter, N),
			findall(K-V, store_get(Store, K, V), All)
		),
		store_close(Store)
	),
	ignore(getenv('INIT', I)),
	format(http_body, "blah: ~w", [I]),
	format(http_body, "Welcome, visitor #~d!", [N]).

http_handler(get("/reset", _), _, _, 204) :-
	store_open(default, Store),
	store_delete(Store, counter),
	store_close(Store).

http_handler(get("/set", ["k"-Key,"v"-Value]), _, _, 200) :-
	setup_call_cleanup(
		store_open(default, Store),
		store_set(Store, Key, Value),
		store_close(Store)
	).

http_handler(get("/a", _), _, _, 200) :-
	html_content,
	%trace,
	'$wasi_kv_open'(default, Store),
	'$wasi_kv_set'(Store, "test", "areawreswrwrewrewrw"),
	%'$wasi_kv_set'(Store, "test", "asdasdasd"),
	'$wasi_kv_get'(Store, "test", X),
	%'$wasi_kv_exists'(Store, "test"),
	'$wasi_kv_delete'(Store, "test"),
	\+ '$wasi_kv_exists'(Store, "test"),
	'$wasi_kv_close'(Store),
	format(http_body, "<b>hello world: ~w ~w</b>", [Store, X]).
	% http_fetch("https://example.com", response(Status, Headers, Body), [method(get)]),
	% write(http_body, 'Fetching example.com...'),
	% format(http_body, "Got status: ~d, Headers: ~w, Body:<br>~s", [Status, Headers, Body]).

http_handler(get("/favicon.ico", _), _, _, 404).

http_handler(_, Path, 404) :-
	text_content,
	format(http_body, "Not found: ~s", [Path]).

http_handler(X, _Hdr, Body, 200) :-
	text_content,
	format(http_body, "hello ~w (body = ~w)", [X, Body]),
	true.