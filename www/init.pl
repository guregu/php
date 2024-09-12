:- use_module(library(spin)).
:- use_module(library(php)).

get('favicon.ico', 404) :-> true.
get('humans.txt', 200) :-> text_content("hello").

spin:http_handler(_, _, _, _) :-
	% let them know
	http_header_set("X-Powered-By", "php"),
	fail.

spin:http_handler(Handle, _Hdr, Body, 200) :-
	php_handle(Handle, Body).

spin:http_handler(_, _, _, 404) :-
	text_content("not found").
