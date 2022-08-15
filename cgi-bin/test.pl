:- use_module(library(cgi)).
:- use_module(library(php)).

main :-
	html_content,
	write_status(200),
	phpinfo.
