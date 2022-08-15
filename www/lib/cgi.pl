:- module(cgi, [env/2, handle/1, html_content/0, text_content/0, query_param/2, write_header/2, write_status/1]).
:- use_module(library(lists)).

:- dynamic(wrote/1).
:- dynamic(query_param/2).
:- initialization(make_params).
make_params :-
	current_prolog_flag(argv, Argv),
	(Argv \= [] -> maplist(make_param, Argv)).
make_param(Arg) :-
	atom_chars(Arg, Cs),
	split(Cs, '=', Key0, Value0),
	Key0 \= [],
	chars_urlenc(Key, Key0, []),
	chars_urlenc(Value, Value0, []),
	% read_term_from_atom(Arg, Key=Value, []),
	assertz(query_param(Key, Value)).

http_error(Status, Msg) :-
	html_content,
	write_status(Status),
	format("<html><h3>error ~w</h3><p>~s~n</p></html>", [Status, Msg]),
	halt.

handle(ScriptName0) :-
	% spin rewrites " to '?
	(  atom(ScriptName0)
	-> atom_chars(ScriptName0, ScriptName)
	;  (
		string(ScriptName0),
		ScriptName = ScriptName0
	   )
	),
	scriptname_file(ScriptName, File),
	catch(handle2(File), Error, (
		html_content,
		maybe_write_status(500),
		write('Error! '),
		write(Error),
		nl
	)).

handle2(File) :-
	atom_concat(_, '.pl', File),
	file_exists(File),
	consult(File),
	!,
	catch(main, Error, (
		html_content,
		maybe_write_status(500),
		write('Error! '),
		write(Error),
		nl
	)),
	halt.

handle2(File) :-
	atom_concat(_, '.html', File),
	atom_concat('public_html', File, F1),
	file_exists(F1),
	html_content,
	write_status(200),
	atom_chars(F1, F2),
	ignore(render(F2)),
	halt.
	
handle("/") :-
	handle2('/index.html') ; handle2('/index.pl').

handle(Path) :-
	format(stderr, "Not found: ~w~n", [Path]),
	http_error(404, "not found"),
	halt.

scriptname_file(Name, File) :-
	chars_urlenc(File, Name, []).

	% ( once(phrase(urlencode(File), Name)) -> true
	% ; http_error(400, "invalid path")
	% ).

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

write_header(Header, Value) :-
	\+wrote(headers),
	\+wrote(header(Header, _)),
	% TODO: sanitize
	format("~a: ~w~n", [Header, Value]),
	assertz(header(Header, Value)),
	!.
write_header(Header, Value) :-
	wrote(headers),
	format(stderr, "Error: already wrote headers! (K: ~w, V: ~w)", [Header, Value]),
	!.
write_header(Header, _) :-
	% TODO: some headers can be sent more than once
	wrote(header(Header, Old)),
	format(stderr, "Error: already wrote header ~w! (V: ~w)", [Header, Old]),
	!.

write_status(Code) :-
	write_header('Status', Code),
	write_headers.

maybe_write_status(Code) :-
	(  \+wrote(headers)
	-> write_status(Code)
	;  true
	).

write_headers :-
	\+wrote(headers),
	nl,
	assertz(wrote(headers)).
write_headers :-
	wrote(headers),
	format(stderr, "Error: already wrote status headers", []).
	% throw(error(duplicate_header_write)).

html_content :- write_header('Content-type', 'text/html; charset=utf-8').
text_content :- write_header('Content-type', 'text/plain; charset=utf-8').
