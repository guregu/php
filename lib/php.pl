:- module(php, [php//1, render/1, phpinfo/0]).
:- use_module(cgi).
:- use_module(dcgs).

php([]) --> [].
php([code(Goal)|Rest]) --> "<?php", whitespace, text(Goal), whitespace, "?>", php(Rest).
php([text(Text)|T]) --> { dif(Text, []) }, text(Text), php(T).
text([<, X|T]) --> [<, X], { dif(X, '?') }, text(T).
text([X|T]) --> [X], { dif(X, '<') }, text(T).
text([]) --> [].
whitespace --> " " | [].

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

exec(code(Code)) :- read_term_from_chars(Code, Goal, []), call(Goal).
% exec(code(Code)) :- format("~s", [Code]).
exec(text([])) :- !.
exec(text(Text)) :- format("~s", [Text]).

phpinfo :-
	format("<style>header { padding: 0.5em; background: #bdb0e1; }~n td:first-of-type { background: #dedeff; }~n td:nth-of-type(2) { background: #dfdfdf; }</style>"),
	pretty_version(Ver),
	format("<header><h1>phpinfo/0<br><span style='font-size: 14pt;'>Version: ~w</span style='font-size: 14pt;'></h1></header>", [Ver]),
	format("<table><h3>Envrionment</h3><tr><th>Name</th><th>Value</th></tr>", []),
	phpinfo_env,
	format("</table>", []),
	format("<table><h3>Prolog flags</h3><tr><th>Flag</th><th>Value</th></tr>", []),
	current_prolog_flag(argv, Argv),
	phpinfo_row(argv-Argv),
	current_prolog_flag(version_git, GitVer),
	phpinfo_row(version_git-GitVer),
	bagof(K-V, current_prolog_flag(K, V), Flags),
	maplist(phpinfo_row, Flags),
	format("</table>", []),
	format("<table><h3>Query params</h3><tr><th>Key</th><th>Value</th></tr>"),
	findall([QK, QV], query_param(QK, QV), QueryParams),
	maplist(format("<tr><td>~w</td><td>~w</td></tr>"), QueryParams),
	format("</table>", []).

phpinfo_env :-
	bagof([K, V], env(K, V), Env),
	maplist(format("<tr><td>~w</td><td>~w</td></tr>"), Env).

phpinfo_row(K-V) :- format("<tr><td>~w</td><td>~w</td></tr>", [K, V]).

pretty_version(Version) :-
	current_prolog_flag(version_data, trealla(Maj, Min, Patch, _)),
	atomic_concat('trealla ', Maj, Head),
	atomic_list_concat([Head, Min, Patch], '.', Version).