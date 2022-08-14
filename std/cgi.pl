:- module(cgi, [env/2, handle/1, html_content/0, text_content/0]).
:- dynamic(wrote/1).

handle(ScriptName) :-
	file_exists(ScriptName),
	consult(ScriptName),
	catch(main, Error, (
		html_content, write('Error! '), write(Error), nl, nl
	)),
	halt.

handle("/") :-
	handle("index.pl"),
	!.

handle(ScriptName) :-
	\+file_exists(ScriptName),
	write_status(404),
	text_content,
	format("404 not found: ~w", [ScriptName]),
	halt.

env(server_protocol, Value) :- getenv('SERVER_PROTOCOL', Value).
env(request_method, Value) :- getenv('REQUEST_METHOD', Value).
env(path_info, Value) :- getenv('PATH_INFO', Value).
env(path_translated, Value) :- getenv('PATH_TRANSLATED', Value).
env(script_name, Value) :- getenv('SCRIPT_NAME', Value).
env(query_string, Value) :- getenv('QUERY_STRING', Value).
env(remote_host, Value) :- getenv('REMOTE_HOST', Value).
env(remote_addr, Value) :- getenv('REMOTE_ADDR', Value).
env(auth_type, Value) :- getenv('AUTH_TYPE', Value).
env(remote_user, Value) :- getenv('REMOTE_USER', Value).
env(remote_ident, Value) :- getenv('REMOTE_IDENT', Value). % unused?
env(content_type, Value) :- getenv('CONTENT_TYPE', Value).

write_content_type(MIME) :-
	\+wrote(content_type(_)),
	format("Content-type: ~w~n~n", [MIME]),
	assertz(wrote(content_type(MIME))),
	!.

write_content_type(_) :-
	wrote(content_type(MIME)),
	format(stderr, "Error: already wrote content-type (~w)~n", [MIME]),
	!.

write_status(Code) :-
	\+wrote(status(_)),
	format("Status: ~w IDK~n", [Code]),
	assertz(wrote(status(Code))),
	!.

write_status(_) :-
	wrote(status(Code)),
	format(stderr, "Error: already wrote status (~w)~n", [Code]),
	!.

html_content :- write_content_type('text/html').
text_content :- write_content_type('text/plain').