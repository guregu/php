:- module(php, [php//1, render/1, phpinfo/0, pretty_version/1, echo/1, htmlspecialchars//1, html_escape/2]).
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

% <?- ... ?> (query)
% <?php ... ?>
exec(php("-", Code)) :-
	exec(php("php", Code)),
	!.
exec(php("php", Code)) :-
	read_term_from_chars(Code, Goal, []),
	ignore(Goal),
	!.
% <?* ... ?> (findall)
exec(php("*", Code)) :-
	read_term_from_chars(Code, Goal, []),
	ignore(findall(_, call(Goal), _)),
	!.
% <?prolog ... ?> (clauses)
% <? ... ?>
exec(php("prolog", Code)) :-
	( once(phrase(clauses(Cs), Code))
	; throw(error(invalid_template(prolog, Code)))
	),
	ignore(maplist(prolog_call, Cs)),
	!.
exec(php([], Code)) :-
	exec(php("prolog", Code)),
	!.
% <?=Var ... ?> (echo)
exec(php([=|Var], Code)) :-
	read_term_from_chars(Code, Goal, [variable_names(Vars)]),
	atom_chars(Key, Var),
	(  member(Key=X, Vars)
	-> true
	;  throw(error(var_not_found(var(Key), goal(Goal))))
	),
	(  call(Goal)
	-> echo(X)
	;  true
	),
	!.
exec(text(Text)) :-
	'$put_chars'(Text),
	!.
exec(X) :-
	throw(error(unknown_opcode(X))).

render(File) :-
	read_file_to_string(File, Cs, []),
	% write(Cs),
	once(phrase(php(Program), Cs)),
	% write(Program).
	catch(maplist(exec, Program), Error, (
		logf("Script error: ~w~nCode: ~w~n", [Error, Program]),
		echo "Error: ", echo Error
	)).

prolog_call(:-(Goal)) :- ignore(Goal).
prolog_call(Goal) :- user:assertz(Goal).

phpinfo :- render('lib/phpinfo.html').

pretty_version(Version) :-
	current_prolog_flag(version_data, trealla(Maj, Min, Patch, _)),
	atomic_concat('trealla ', Maj, Head),
	atomic_list_concat([Head, Min, Patch], '.', Version).

htmlspecialchars([]) --> [].
htmlspecialchars([C|Cs]) --> { danger_subtitute(C, Sub) }, Sub, htmlspecialchars(Cs).
htmlspecialchars([C|Cs]) --> { \+danger_subtitute(C, _) }, [C], htmlspecialchars(Cs).

html_escape(Raw, Sanitized) :- once(phrase(htmlspecialchars(Raw), Sanitized)).

danger_subtitute(&, "&amp;").
danger_subtitute('"', "&quot;").
danger_subtitute('\'', "&apos;").
danger_subtitute(<, "&lt;").
danger_subtitute(>, "&gt;").

echo([]) :- !.
echo('') :- !.
echo(String) :-
	can_be(chars, String),
	html_escape(String, Sanitized),
	'$put_chars'(Sanitized),
	!.
echo(X) :-
	write_term_to_chars(X, [], Cs),
	echo(Cs),
	!.
	