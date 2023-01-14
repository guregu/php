:- module(cgi, [env/2, handle/1, html_content/0, text_content/0, query_param/2, write_header/2, write_status/1, logf/2, debugf/2]).
:- use_module(library(lists)).
:- use_module(library(format)).

:- dynamic(wrote/1).
:- dynamic(query_param/2).
:- initialization(make_params).

make_params :-
	env(request_method, Method),
	logf("Method: ~w", [Method]),
	flush_output(stderr),
	ignore(make_params(Method)).

make_params('POST') :-
	% TODO: handle other mime types
	env(content_length, N),
	read_len(N, Line),
	make_post_params(Line).

make_post_params(Line) :-
	( nonvar(Line) -> true ; throw(error(invalid(Line))) ),
	logf("Parsing POST: ~w~n", [Line]),
	once(phrase(form(Params), Line)),
	logf("Post params: ~q~n", [Params]),
	maplist(assert_param, Params).

make_params('GET') :-
	current_prolog_flag(argv, Argv),
	logf("Parsing get: ~w~n", [Argv]),
	(Argv \= [] -> maplist(parse_get_param, Argv) ; true).
make_params(_).

assert_param(K-V) :- assertz(query_param(K, V)).

parse_get_param(Arg) :-
	atom_chars(Arg, Cs),
	split(Cs, '=', Key0, Value0),
	Key0 \= [],
	chars_urlenc(Key, Key0, []),
	(  chars_urlenc(Value, Value0, [])
	-> true
	;  Value = ''
	),
	assert_param(Key-Value).

handle(ScriptName) :-
	scriptname_file(ScriptName, File),
	catch(handle_file(File), Error, handle_error(Error)).
	
handle("/") :-
	handle("/index.html") ; handle("/index.pl").

handle(Path) :-
	format(stderr, "Not found: ~w~n", [Path]),
	http_error(404, "not found"),
	halt.

handle_file(File) :-
	logf("F1: ~w", [File]),
	atom_concat(_, '.pl', File),
	file_exists(File),
	consult(File),
	assertz(current_file(File)),
	logf("Handling script file: ~w", [File]),
	!,
	catch(main, Error, handle_error(Error)),
	flush_output,
	halt.

handle_file(File) :-
	atom_concat(_, '.html', File),
	atom_concat('public_html', File, Path),
	file_exists(Path),
	assertz(current_file(Path)),
	logf("Handling script file: ~w", [File]),
	!,
	html_content,
	write_status(200),
	atom_chars(Path, Template),
	call_cleanup(render(Template), (flush_output, halt)).

handle_file(File) :-
	logf("Handling static file ~w", [File]),
	atom_concat('public_html', File, Path),
	logf("File: ~w", [Path]),
	!,
	file_exists(Path),
	time_file(Path, LastMod),
	once(phrase(format_("\"~f\"", [LastMod]), LMs)),
	atom_chars(LM, LMs),
	% TODO: make this less horrible
	(  env(if_none_match, LM)
	-> write_status(304), flush_output, halt
	;  true
	),
	split_string(Path, '.', '', Split),
	last(Split, Ext),
	once(ext_mime(Ext, Mime)),
	logf("ext: ~w mime: ~w lm: ~w", [Ext, Mime, LastMod]),
	mime_content(Mime),
	write_header('ETag', LM),
	write_status(200),
	read_file_to_string(Path, X, []),
	'$put_chars'(X),
	flush_output,
	halt.

handle_error(Error) :-
	logf("Error! ~w", [Error]),
	html_content,
	maybe_write_status(500),
	format("Error: ~w~n", [Error]).

scriptname_file(Name, File) :-
	chars_urlenc(File, Name, []).

http_error(Status, Msg) :-
	logf("Returning HTTP error ~w: ~w", [Status, Msg]),
	html_content,
	write_status(Status),
	format("<html><h3>error ~w</h3><p>~s~n</p></html>", [Status, Msg]),
	halt.

env(server_protocol, Value) :- getenv('SERVER_PROTOCOL', Value).
env(request_method, Value) :- getenv('REQUEST_METHOD', Value).
env(path_info, Value) :- getenv('PATH_INFO', Value).
env(path_translated, Value) :- getenv('PATH_TRANSLATED', Value).
env(script_name, Value) :- getenv('SCRIPT_NAME', Value).
env(query_string, Value) :- getenv('QUERY_STRING', Value).
env(post_string, Value) :- getenv('POST_STRING', Value).
env(remote_host, Value) :- getenv('REMOTE_HOST', Value).
env(remote_addr, Value) :- getenv('REMOTE_ADDR', Value).
env(auth_type, Value) :- getenv('AUTH_TYPE', Value).
env(remote_user, Value) :- getenv('REMOTE_USER', Value).
env(remote_ident, Value) :- getenv('REMOTE_IDENT', Value). % unused?
env(content_type, Value) :- getenv('CONTENT_TYPE', Value).
env(content_length, Value) :- getenv('HTTP_CONTENT_LENGTH', N), nonvar(N), atom_chars(N, Ns), number_chars(Value, Ns).
env(if_none_match, Value) :- getenv('HTTP_IF_NONE_MATCH', Value).

write_header(Header, Value) :-
	\+wrote(headers),
	\+wrote(header(Header, _)),
	% TODO: sanitize
	format("~a: ~w~n", [Header, Value]),
	assertz(header(Header, Value)),
	!.
write_header(Header, Value) :-
	wrote(headers),
	logf("Error: already wrote headers! (K: ~w, V: ~w)", [Header, Value]),
	!.
write_header(Header, _) :-
	% TODO: some headers can be sent more than once
	wrote(header(Header, Old)),
	logf("Error: already wrote header ~w! (V: ~w)", [Header, Old]),
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
	logf("Error: already wrote status headers", []).
	% throw(error(duplicate_header_write)).

html_content :- write_header('Content-type', 'text/html; charset=utf-8').
text_content :- write_header('Content-type', 'text/plain; charset=utf-8').
json_content :- write_header('Content-type', 'application/json; charset=utf-8').
binary_content :- write_header('Content-type', 'application/octet-stream').
mime_content(Mime) :- write_header('Content-type', Mime).

ext_mime(js, 'application/javascript').
ext_mime(txt, 'text/plain; charset=utf-8').
ext_mime(pl, 'application/x-prolog; charset=utf-8').
ext_mime(png, 'image/png').
ext_mime(gif, 'image/gif').
ext_mime(jpeg, 'image/jpeg').
ext_mime(jpg, 'image/jpeg').
ext_mime(_, 'application/octet-stream').

logf(Fmt, Args) :-
	date_time(_, _, _, HH, MM, S),
	format(stderr, "[~|~`0t~w~2+:~|~`0t~w~2+:~`0t~w~2+] ", [HH, MM, S]),
	format(stderr, Fmt, Args),
	write(stderr, '\n'),
	flush_output(stderr).

debugf(_, _).

form([V|Vs]) --> param(V), params(Vs).
params([V|Vs]) --> "&", param(V), params(Vs).
params([]) --> [].
param(K-V) --> value(K0), "=", value(V0), { atom_chars(K1, K0), chars_urlenc(K, K1, []), atom_chars(V1, V0), chars_urlenc(V, V1, []) }.
value([V|Vs]) --> { dif(V, =), dif(V, []) }, [V], value(Vs).
value([]) --> [].

read_len(Len, Cs) :-
	read_len_(0, Len, Cs).
read_len_(N0, Len, [C|Cs]) :-
	N0 < Len,
	get_char(C),
	succ(N0, N),
	read_len_(N, Len, Cs).
read_len_(Len, Len, []).