:- use_module(library(spin)).
:- use_module(library(php)).

:- multifile([spin:http_handler/4]).

get("/favicon.ico", 404) :-> true.

spin:http_handler(Handle, _Hdr, Body, 200) :-
    php_handle(Handle, Body).

spin:http_handler(H, _, _, 404) :-
    html_content,
    format(http_body, "Not found: ~w~n", [H]).
