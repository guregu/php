:- use_module(library(cgi)).

main :-
	text_content, write_status(200), write('oh hi mark'), nl.