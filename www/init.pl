:- use_module(library(spin)).

http_handler(get("/", _), _, _, 200) :-
	html_content,
	format(http_body, "<b>hello world</b>", []).
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