:- module(php, [php//1, render/1, phpinfo/0, pretty_version/1, echo/1, htmlspecialchars//1, html_escape/2, op(901, fy, echo)]).
:- use_module(cgi).
:- use_module(library(dcgs)).

:- op(901, fy, echo).

% php//1 grammar lexes script text into tokens
php([H|T]) --> block(H), php(T).
php([]) --> [].

block(php(Head, [])) --> "<?", nonwhite(Head), whitespace, "?>".
block(php(Head, Text)) --> "<?", nonwhite(Head), whitespace, text(Text), whitespace, "?>".
block(php("prolog", Text)) --> "<?", whitespace, text(Text), whitespace, "?>".
block(text(Text)) --> text(Text), { Text \= [] }.

text(['<', X|T]) --> ['<', X], { dif(X, '?') }, text(T).
text([X|T]) --> [X], { dif(X, '<') }, text(T).
text([]) --> [].
text("<") --> "<".

nonwhite([X|Cs]) --> { dif(X, '\n'), dif(X, '\t'), dif(X, ' ') },  [X], { atom(X), \+char_type(X, white) }, nonwhite(Cs).
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
	{
		dif(Head, "if"),
		dif(Head, "findall"),
		dif(Head, "end")
	},
	[php(Head, Text)],
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

exec_capture :-
	exec_flush,
	nb_setval(capture, safe),
	'$capture_output'.

exec_flush :-
	nb_current(capture, safe),
	'$capture_output_to_chars'(Cs),
	ignore(echo(Cs)),
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
	ignore(findall(_, call(Goal), _)).

% <?prolog ... ?> (clauses)
% <? ... ?>
exec_(Vars, php("prolog", Code)) :-
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
	% write(Cs),
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
	atomic_concat('trealla ', Maj, Head),
	atomic_list_concat([Head, Min, Patch], '.', Version).

htmlspecialchars([]) --> [].
htmlspecialchars([C|Cs]) --> { danger_substitute(C, Sub) }, Sub, htmlspecialchars(Cs).
htmlspecialchars([C|Cs]) --> { \+danger_substitute(C, _) }, [C], htmlspecialchars(Cs).

html_escape(Raw, Sanitized) :- once(phrase(htmlspecialchars(Raw), Sanitized)).

danger_substitute(&, "&amp;").
danger_substitute('"', "&quot;").
danger_substitute('\'', "&apos;").
danger_substitute(<, "&lt;").
danger_substitute(>, "&gt;").

echo([]) :- !.
echo('') :- !.
echo(String) :-
	string(String),
	html_escape(String, Sanitized),
	'$put_chars'(Sanitized),
	!.
echo(X) :-
	write_term_to_chars(X, [], Cs),
	echo(Cs),
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