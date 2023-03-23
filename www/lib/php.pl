:- module(php, [php//1, render/1, phpinfo/0, pretty_version/1, echo/1, htmlspecialchars//1, html_escape/2, op(901, fy, echo), env/2, query_param/2]).
:- use_module(library(dcgs)).
:- use_module(library(format)).

:- dynamic(env/2).
:- dynamic(query_param/2).
:- dynamic(form_value/2).

:- op(901, fy, echo).

php_handle(Handle, Body) :-
	Handle =.. [_Method, Path, Params],
	file_root(Root),
	once(phrase(filename(Path), File0)),
	append(Root, File0, File),
    html_content,
    assertz(current_file(File)),
	assert_params(query_param, Params),
	(  Body = form(Pairs)
    -> assert_params(form_value, Pairs)
    ;  true
    ),
	setup_call_cleanup(
		current_output(S),
		( set_output(http_body), render(File) ),
		set_output(S)
	).

assert_params(Functor, Ps) :- maplist(assert_param(Functor), Ps).
assert_param(Functor, K-V) :-
	X =.. [Functor, K, V],
	assertz(X).

% php//1 grammar lexes script text into tokens
php([H|T]) --> block(H), php(T).
php([]) --> [].

block(php(Head, [])) --> "<?", nonwhite(Head), whitespace, "?>".
block(php(Head, Text)) --> "<?", nonwhite(Head), whitespace, code(Text), whitespace, "?>".
block(php("prolog", Text)) --> "<?", whitespace, code(Text), whitespace, "?>".
block(text(Text)) --> text(Text), { Text \= [] }.

text([H|T]) --> [H], { H == '<' }, text_nonquestion(T).
text([H|T]) --> [H], { H \= '<' }, text(T).
text([]) --> [].
text_nonquestion([H|T]) --> [H], { H \= '?' }, text(T).

code([H|T]) --> [H], { H == '?' }, code_nonbracket(T).
code([H|T]) --> [H], { H \= '?' }, code(T).
code([]) --> [].
code_nonbracket([H|T]) --> [H], { H \= '>' }, code(T).

nonwhite([X|Cs]) --> [X], { atom(X), \+char_type(X, white), X \= '\n', X \= '\t' }, nonwhite(Cs).
nonwhite([]) --> [].

whitespace --> [X], { atom(X), char_type(X, white) }, (whitespace | []).
whitespace --> " " | "\n" | "\t".

clauses(Cs) --> "%", seq(_), "\n", clauses(Cs). % ignore comments
clauses([X|Xs]) --> term(X), { X \= end_of_file }, clauses(Xs).
clauses([]) --> term(end_of_file).
term(T) --> read_term_from_chars_(T).

% program//1 grammar parses tokens from php//1 into goals for exec/2.
program([findall(Goal, Blocks)|Xs]) -->
	[php("findall", Goal)],
	program(Blocks),
	[php("end", [])],
	program(Xs).
program([if(Goal, Blocks)|Xs]) -->
	[php("if", Goal)],
	program(Blocks),
	[php("end", [])],
	program(Xs).
program([php(Head, Text)|Xs]) -->
	[php(Head, Text)],
	{
		Head \= "if",
		Head \= "findall",
		Head \= "end"
	},
	program(Xs).
program([text(Text)|Xs]) -->
	[text(Text)], program(Xs).
program([]) --> [].

exec(Block) :-
	exec([], Block).

exec(Vars, Block) :-
	\+unsafe_block(Block),
	exec_capture,
	ignore(exec_(Vars, Block)),
	exec_flush,
	!.
exec(Vars, Block) :-
	unsafe_block(Block),
	exec_flush,
	ignore(exec_(Vars, Block)).

% For exec_capture/0 and exec_flush/0 we need to do some ugly extralogical things
% to avoid double-capture, which would result in the output being out-of-order.

% exec_capture :- true.
exec_capture :-
	exec_flush,
	current_output(S),
	nb_setval(capture, S),
	'$memory_stream_create'(Cap, []),
	set_output(Cap).

exec_flush :-
	nb_current(capture, S),
	current_output(Cap),
	'$memory_stream_to_chars'(Cap, Cs),
	close(Cap),
	ignore(echo(S, Cs)),
	set_output(S),
	nb_delete(capture),
	flush_output,
	!.
exec_flush :- flush_output.

% <?- ... ?> (query)
% <?php ... ?>
exec_(Vars0, php("php", Code)) :-
	read_term_from_chars(Code, Goal, [variable_names(Vars1)]),
	merge_vars(Vars0, Vars1, _),
	ignore(Goal).
exec_(Vars, php("-", Code)) :-
	exec_(Vars, php("php", Code)).
exec_(Vars, php("unsafe", Code)) :-
	exec_(Vars, php("php", Code)).

% <?* ... ?> (findall)
exec_(Vars0, php("*", Code)) :-
	read_term_from_chars(Code, Goal, [variable_names(Vars1)]),
	merge_vars(Vars0, Vars1, _),
	findall(_, call(Goal), _).

% <?prolog ... ?> (clauses)
% <? ... ?>
exec_(_Vars, php("prolog", Code)) :-
	( once(phrase(clauses(Cs), Code))
	; throw(error(invalid_template(prolog, Code)))
	),
	ignore(maplist(prolog_call, Cs)).
exec_(Vars, php([], Code)) :-
	exec_(Vars, php("prolog", Code)).

% <?=Var ... ?> (echo)
exec_(Vars0, php([=|Var], Code)) :-
	Code \= [],
	read_term_from_chars(Code, Goal, [variable_names(Vars1)]),
	merge_vars(Vars0, Vars1, Vars),
	atom_chars(Key, Var),
	(  memberchk(Key=X, Vars)
	-> true
	;  throw(error(var_not_found(var(Key), goal(Goal))))
	),
	(  call(Goal)
	-> echo_unsafe(X)
	;  true
	).

% <?=Var ?> (shorthand for <?=Var true. ?>)
exec_(Vars, php([=|Var], [])) :-
	atom_chars(Key, Var),
	(  memberchk(Key=X, Vars)
	-> true
	;  throw(error(var_not_found(var(Key))))
	),
	echo_unsafe(X).

% <?if ... ?> ... <?end ?> (if blocks)
exec_(Vars0, if(Condition, Blocks)) :-
	read_term_from_chars(Condition, Cond, [variable_names(Vars1)]),
	merge_vars(Vars0, Vars1, Vars),
	(  call(Cond)
	-> maplist(exec(Vars), Blocks)
	;  true
	).

% <?findall ... ?> ... <?end ?> (findall blocks)
exec_(Vars0, findall(G, Blocks)) :-
	read_term_from_chars(G, Goal, [variable_names(Vars1)]),
	merge_vars(Vars0, Vars1, Vars),
	findall(_, (call(Goal), maplist(exec(Vars), Blocks)), _).

% raw text
exec_(_, text(Text)) :-
	'$put_chars'(Text).

exec_(Vars, X) :-
	throw(error(unknown_block(X, vars(Vars)))).

unsafe_block(text(_)).
unsafe_block(php("unsafe", _)).

merge_vars(L0, L1) :-
	maplist(merge_vars_(L1), L0).
merge_vars_(L, Name=Var) :-
	(  memberchk(Name=V1, L)
	-> Var = V1
	;  true
	).

merge_vars(Vs0, Vs1, Vs) :-
	merge_vars(Vs0, Vs1),
	merge_vars(Vs1, Vs0),
	once(union(Vs0, Vs1, Vs)).

render(File) :-
	read_file_to_string(File, Cs, []),
	once(phrase(php(PHP), Cs)),
	(  phrase(program(Program), PHP)
	-> true
	;  throw(error(invalid_program(PHP)))
	),
	!,
	% logf("~nPHP: ~w~nProg: ~w~n", [PHP, Program]),
	catch(maplist(exec, Program), Error, (
		logf("Script error: ~w~nCode: ~w~n", [Error, Program]),
		echo "Error: ", echo Error
	)).

prolog_call(':-'(Goal)) :- ignore(Goal).
prolog_call(Goal) :- user:assertz(Goal).

phpinfo :- render('lib/phpinfo.html').

pretty_version(Version) :-
	current_prolog_flag(version_data, trealla(Maj, Min, Patch, _)),
	once(phrase(format_("trealla ~d.~d.~d", [Maj, Min, Patch]), Version)).

htmlspecialchars([]) --> [].
htmlspecialchars([C|Cs]) --> { danger_substitute(C, Sub) }, Sub, htmlspecialchars(Cs).
htmlspecialchars([C|Cs]) --> { \+danger_substitute(C, _) }, [C], htmlspecialchars(Cs).

html_escape(Raw, Sanitized) :- once(phrase(htmlspecialchars(Raw), Sanitized)).

danger_substitute(&, "&amp;").
danger_substitute('"', "&quot;").
danger_substitute('\'', "&apos;").
danger_substitute(<, "&lt;").
danger_substitute(>, "&gt;").

echo(X) :- current_output(S), echo(S, X).
echo(_, []) :- !.
echo(_, '') :- !.
echo(S, String) :-
	string(String),
	html_escape(String, Sanitized),
	'$put_chars'(S, Sanitized),
	!.
echo(S, X) :-
	write_term_to_chars(X, [], Cs),
	echo(S, Cs),
	!.
	
echo_unsafe([]) :- !.
echo_unsafe('') :- !.
echo_unsafe(String) :-
	string(String),
	'$put_chars'(String),
	!.
echo_unsafe(X) :-
	write_term_to_chars(X, [], Cs),
	echo_unsafe(Cs),
	!.

logf(Fmt, Args) :-
	date_time(_, _, _, HH, MM, S),
	format(stderr, "[~|~`0t~w~2+:~|~`0t~w~2+:~`0t~w~2+] ", [HH, MM, S]),
	format(stderr, Fmt, Args),
	write(stderr, '\n'),
	flush_output(stderr).

debugf(_, _).

file_root(Root) :-
    file_root_(Root), 
    append(_, "/", Root).
file_root(Root) :-
    file_root_(Root0), 
    append(Root0, "/", Root).
file_root_(Root) :- getenv('PHP_ROOT', Root0), atom_chars(Root0, Root).
file_root_("public_html").

filename([]) --> "index.html".
filename([H|T]) -->
    { atom_chars(H, Cs) },
    Cs,
    filename_more(T).
filename_more([H|T]) --> "/", { atom_chars(H, Cs) }, Cs, filename_more(T).
filename_more([]) --> [].