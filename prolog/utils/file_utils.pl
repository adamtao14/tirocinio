/** <file_utils> Utilità per la gestione dei file
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per la gestione dei file (lettura, scrittura,
 *  cancellazione)
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(file_utils, [
	read_lines/2,
	read_lines_if_exists/2,
	read_lines_from_stream/2,
	write_lines/2,
	append_lines/2,
	delete_file_safely/1
]).

:- use_module(library(readutil)).

%% read_lines(+Filename:string, -Lines:list) is det
%
%  Il predicato read_lines/2 apre il file Filename, lo
%  legge e produce una lista Lines contenente le sue righe.
%
%  @param Filename Il percorso del file da leggere.
%  @param Lines La lista contenente le righe del file.
%
read_lines(Filename, Lines) :-
	setup_call_cleanup(
		open(Filename, read, Stream),
		read_lines_from_stream(Stream, Lines),
		close(Stream)).

%% read_lines_if_exists(+Filename:string, -Lines:list) is det
%
%  Il predicato read_lines_if_exists/2 apre il file
%   Filename, lo legge e produce una lista Lines
%   contenente le sue righe. Se il file non esiste,
%   si ritorna la lista vuota.
%
%  @param Filename Il percorso del file da leggere.
%  @param Lines La lista contenente le righe del file.
%
read_lines_if_exists(Filename, Lines) :-
	( exists_file(Filename) -> read_lines(Filename, Lines)
	; Lines = []
	).

%% read_lines_from_stream(+Stream:stream, -Lines:list) is det
%
%  Il predicato read_lines_from_stream/2 apre lo stream
%  Stream, lo legge riga per riga e produce una lista
%  Lines contenente le righe lette.
%
%  @param Stream Lo stream da cui avviene la lettura
%  @param Lines La lista contenente le righe del file.
%
read_lines_from_stream(Stream, []) :-
	at_end_of_stream(Stream), !.
read_lines_from_stream(Stream, [Line|Rest]) :-
	\+ at_end_of_stream(Stream),
	read_line_to_string(Stream, Line),
	read_lines_from_stream(Stream, Rest).

%% write_lines(+Filename:string, +Lines:list) is det
%
%  Il predicato write_lines/2 apre il file Filename e
%  lo sovrascrive con le righe contenute nella lista
%  Lines.
%
%  @param Filename Il percorso del file da scrivere.
%  @param Lines La lista contenente le righe del file.
%
write_lines(Filename, Lines) :-
	setup_call_cleanup(
		open(Filename, write, Stream),
		forall(member(Line, Lines), writeln(Stream, Line)),
		close(Stream)).

%% append_lines(+Filename:string, +Lines:list) is det
%
%  Il predicato write_lines/2 apre il file Filename e
%  appende in scrittura le righe contenute nella lista
%  Lines.
%
%  @param Filename Il percorso del file da scrivere.
%  @param Lines La lista contenente le righe del file.
%
append_lines(Filename, Lines) :-
	setup_call_cleanup(
		open(Filename, append, Stream),
		forall(member(Line, Lines), writeln(Stream, Line)),
		close(Stream)).

%% delete_file_safely(+Filename:string) is det
%
%  Il predicato delete_file_safely/1 cancella il file
%  Filename. Se il file di nome Filename non esiste,
%  ritorna semplicemente true.
%  Questo predicato esegue con privilegi normali.
%  Per cancellare file con i privilegi di root è
%  necessario eseguire il comando UNIX rm con tali
%  privilegi.
%
%  @param Filename Il percorso del file da cancellare.
%
delete_file_safely(Filename) :-
	( exists_file(Filename) -> delete_file(Filename) ; true ).
