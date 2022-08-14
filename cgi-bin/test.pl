:- use_module(cgi).
:- use_module(php).

main :-
	html_content,
	phpinfo.
