:- module(php, [phpinfo/0]).
:- use_module(cgi).

phpinfo :-
	format("<style>header { padding: 0.5em; background: #bdb0e1; }~n td:first-of-type { background: #dedeff; }~n td:nth-of-type(2) { background: #dfdfdf; }</style>"),
	current_prolog_flag(version_data, Ver),
	format("<header><h1>phpinfo/0<br><span style='font-size: 14pt;'>Version: ~w</span style='font-size: 14pt;'></h1></header>", [Ver]),
	format("<table><h3>Envrionment</h3><tr><th>Name</th><th>Value</th></tr>", []),
	phpinfo_env,
	format("</table>", []),
	format("<table><h3>Prolog flags</h3><tr><th>Flag</th><th>Value</th></tr>", []),
	current_prolog_flag(argv, Argv),
	phpinfo_row(argv-Argv),
	bagof(K-V, current_prolog_flag(K, V), Flags),
	maplist(phpinfo_row, Flags),
	format("</table>", []).

phpinfo_env :-
	bagof([K, V], env(K, V), Env),
	maplist(format("<tr><td>~w</td><td>~w</td></tr>"), Env).

phpinfo_row(K-V) :- format("<tr><td>~w</td><td>~w</td></tr>", [K, V]).
