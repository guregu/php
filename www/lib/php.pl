:- module(php, [php//1, render/1, phpinfo/0, pretty_version/1, echo/1, htmlspecialchars//1, html_escape/2, op(901, fy, echo)]).
:- use_module(cgi).
:- use_module(dcgs).

:- op(901, fy, echo).

php([H|T]) --> block(H), php(T).
php([]) --> [].

block(php(Head, Text)) --> "<?", seq(Head), whitespace, seq(Text), whitespace, "?>".
block(text(Text)) --> text(Text), { Text \= [] }.

text(['<', X|T]) --> ['<', X], { dif(X, '?') }, text(T).
text([X|T]) --> [X], { dif(X, '<') }, text(T).
text([]) --> [].
text("<") --> "<".

whitespace --> [X], { atom(X), char_type(X, white) }, (whitespace | []).
whitespace --> " " | "\n" | "\t".

clauses(Cs) --> "%", seq(_), "\n", clauses(Cs). % ignore comments
clauses([X|Xs]) --> term(X), { X \= end_of_file }, clauses(Xs).
clauses([]) --> term(end_of_file).
term(T) --> read_term_from_chars_(T).

exec(Block) :-
	\+unsafe_block(Block),
	'$capture_output',
	ignore(exec_(Block)),
	'$capture_output_to_chars'(Cs),
	ignore(echo(Cs)), % escapes output
	!.
exec(Block) :-
	unsafe_block(Block),
	ignore(exec_(Block)).

% <?- ... ?> (query)
% <?php ... ?>
exec_(php("-", Code)) :-
	exec_(php("php", Code)),
	!.
exec_(php("php", Code)) :-
	read_term_from_chars(Code, Goal, []),
	ignore(Goal),
	!.
exec_(php("unsafe", Code)) :-
	exec_(php("php", Code)),
	!.
% <?* ... ?> (findall)
exec_(php("*", Code)) :-
	read_term_from_chars(Code, Goal, []),
	ignore(findall(_, call(Goal), _)),
	!.
% <?prolog ... ?> (clauses)
% <? ... ?>
exec_(php("prolog", Code)) :-
	( once(phrase(clauses(Cs), Code))
	; throw(error(invalid_template(prolog, Code)))
	),
	ignore(maplist(prolog_call, Cs)),
	!.
exec_(php([], Code)) :-
	exec_(php("prolog", Code)),
	!.
% <?=Var ... ?> (echo)
exec_(php([=|Var], Code)) :-
	read_term_from_chars(Code, Goal, [variable_names(Vars)]),
	atom_chars(Key, Var),
	(  member(Key=X, Vars)
	-> true
	;  throw(error(var_not_found(var(Key), goal(Goal))))
	),
	(  call(Goal)
	-> echo_unsafe(X)
	;  true
	),
	!.
exec_(text(Text)) :-
	'$put_chars'(Text),
	!.
exec_(X) :-
	throw(error(unknown_block(X))).

unsafe_block(text(_)).
unsafe_block(php("unsafe", _)).

render(File) :-
	read_file_to_string(File, Cs, []),
	% write(Cs),
	once(phrase(php(Program), Cs)),
	% write(Program).
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