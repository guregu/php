:- use_module(library(spin)).
:- use_module(library(php)).

get('favicon.ico', 404) :-> true.
get('humans.txt', 200) :-> text_content("hello").

spin:http_handler(Handle, _Hdr, Body, 200) :-
	php_handle(Handle, Body).
