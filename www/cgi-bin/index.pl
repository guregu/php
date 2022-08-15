:- use_module(library(php)).

main :-
	html_content,
	write_status(200),
	render("public_html/index.html").

main_old :-
	html_content,
	write_status(200),
	format("<html><marquee style='font-size: 96pt;'><i>PHP: Web Framework Of The Future</i></marquee></html>~n", []).
