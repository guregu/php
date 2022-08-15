:- module(php, [php//1, render/1, phpinfo/0, pretty_version/1]).
:- use_module(cgi).
:- use_module(dcgs).

php([]) --> [].
php([code(Goal)|Rest]) --> "<?", ("php" | []), whitespace, text(Goal), whitespace, "?>", !, php(Rest).
php([text(Text)|T]) --> text(Text), { Text \= [] }, php(T).
text([<, X|T]) --> [<, X], { dif(X, '?') }, !, text(T).
text([X|T]) --> [X], { dif(X, '<') }, text(T).
text([]) --> [].
text([<]) --> [<].

whitespace --> [X], { char_type(X, white) }, whitespace.
whitespace --> [].

render(File) :-
	read_file_to_string(File, Cs, []),
	once(phrase(php(Program), Cs)),
	maplist(exec, Program).
	% ( setup_call_cleanup(
	% 	open(F, read, _, [mmap(Cs)]),
	% 	(write(Cs), phrase(php(Program), Cs)),
	% 	true
	% ) -> true ; throw(asjdksadkasdas) ),
	% maplist(exec, Program),
	% close(S).

exec(code(Code)) :- read_term_from_chars(Code, Goal, []), ignore(call(Goal)). % 🐘
exec(text([])) :- !.
exec(text(Text)) :- format("~s", [Text]).

phpinfo :-
	render('lib/phpinfo.html').

phpinfo_env :-
	bagof([K, V], env(K, V), Env),
	maplist(format("<tr><td>~w</td><td>~w</td></tr>"), Env).

phpinfo_row(K-V) :- format("<tr><td>~w</td><td>~w</td></tr>", [K, V]).

pretty_version(Version) :-
	current_prolog_flag(version_data, trealla(Maj, Min, Patch, _)),
	atomic_concat('trealla ', Maj, Head),
	atomic_list_concat([Head, Min, Patch], '.', Version).