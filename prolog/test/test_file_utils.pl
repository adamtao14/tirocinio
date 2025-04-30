:- begin_tests(file_utils).
:- use_module(utils/file_utils).

%% Test: lettura di un file per righe
%  Verifica che `read_lines/3` sia in grado di leggere un
%  file per righe e popolare la lista Lines con ogni riga
%  letta.
test(read_lines) :-
	Filename = "test/file-read.txt",
	DesiredLines = ["uno", "due", "tre"],
	read_lines(Filename, Lines),
	assertion(Lines = DesiredLines).

%% Test: lettura di un file per righe
%  Verifica che `read_lines_if_exists/2` sia in grado di
%  leggere un file per righe e popolare la lista Lines con
%  ogni riga letta.
%  Se il file non esiste, ritorna la lista vuota.
%
test(read_lines_if_nonexistent_file) :-
	Filename = "nonexistent",
	DesiredLines = [],
	read_lines_if_exists(Filename, Lines),
	assertion(Lines = DesiredLines).

%% Test: lettura di un file per righe
%  Verifica che `read_lines_if_exists/2` sia in grado di
%  leggere un file per righe e popolare la lista Lines con
%  ogni riga letta.
%  Se il file esiste, ritorna la lista contenente le righe
%  del file.
%
test(read_lines_if_existent_file) :-
	Filename = "test/file-read.txt",
	DesiredLines = ["uno", "due", "tre"],
	read_lines_if_exists(Filename, Lines),
	assertion(Lines = DesiredLines).

%% Test: lettura di uno stream per righe
%  Verifica che `read_lines_from_stream/2` sia in grado di
%  leggere uno stream per righe da una stringa e ritornare
%  una lista Lines contenente le righe del file.
%  Se lo stream è vuoto, la lista deve essere vuota.
test(read_lines_from_empty_string_stream) :-
	DesiredLines = [],
	open_string("", Stream),
	read_lines_from_stream(Stream, Lines),
	close(Stream),
	assertion(Lines = DesiredLines).

%% Test: lettura di uno stream per righe
%  Verifica che `read_lines_from_stream/2` sia in grado di
%  leggere uno stream per righe da una stringa e ritornare
%  una lista Lines contenente le righe del file.
%  Se lo stream è lungo una riga, la lista deve essere
%  composta da un solo elemento (l'unica riga).
test(read_lines_from_one_line_string_stream) :-
	DesiredLines = ["one"],
	open_string("one", Stream),
	read_lines_from_stream(Stream, Lines),
	close(Stream),
	assertion(Lines = DesiredLines).

%% Test: lettura di uno stream per righe
%  Verifica che `read_lines_from_stream/2` sia in grado di
%  leggere uno stream per righe da una stringa e ritornare
%  una lista Lines contenente le righe del file.
%  Se lo stream è lungo più righe, la lista deve essere
%  composta da più elementi (uno per riga letta).
test(read_lines_from_multiple_lines_string_stream) :-
	DesiredLines = ["one", "two", "three"],
	open_string("one\ntwo\nthree", Stream),
	read_lines_from_stream(Stream, Lines),
	close(Stream),
	assertion(Lines = DesiredLines).

%% Test: lettura di uno stream per righe
%  Verifica che `read_lines_from_stream/2` sia in grado di
%  leggere uno stream per righe da un file e ritornare
%  una lista Lines contenente le righe del file.
%  Se lo stream è lungo più righe, la lista deve essere
%  composta da più elementi (uno per riga letta).
test(read_lines_from_multiple_lines_file_stream) :-
	DesiredLines = ["uno", "due", "tre"],
	open("test/file-read.txt", read, Stream),
	read_lines_from_stream(Stream, Lines),
	close(Stream),
	assertion(Lines = DesiredLines).

%% Test: scrittura di un file per righe
%  Verifica che `write_lines/2` sia in grado di
%  sovrascrivere un file per righe con i contenuti della
%  lista Lines.
test(write_lines) :-
	Filename = "test/file-write.txt",
	DesiredLines = ["uno", "due", "tre"],
	write_lines(Filename, DesiredLines),
	read_lines(Filename, Lines),
	assertion(Lines = DesiredLines).

%% Test: scrittura in append di un file per righe
%  Verifica che `append_lines/2` sia in grado di
%  appendere alcune righe ad un file con i contenuti
%  della lista Lines.
test(append_lines) :-
	Filename = "test/file-write.txt",
	AppendLines = ["quattro", "cinque", "sei"],
	DesiredLines = ["uno", "due", "tre", "quattro", "cinque", "sei"],
	append_lines(Filename, AppendLines),
	read_lines(Filename, Lines),
	assertion(Lines = DesiredLines).

%% Test: cancellazione di un file inesistente
%  Verifica che `delete_file/1` ritorni true
%  se il file non esiste.
test(delete_nonexisting_file) :-
	Filename = "nonesistente",
	assertion(delete_file_safely(Filename)).

%% Test: cancellazione di un file esistente
%  Verifica che `delete_file/1` cancelli un
%  file esistente e ritorni true.
test(delete_existing_file) :-
	Filename = "test/file-write.txt",
	assertion(exists_file(Filename)),
	assertion(delete_file_safely(Filename)),
	assertion(\+ exists_file(Filename)).

%, [setup(delete_file_safe('test_lines.txt')), cleanup(delete_file_safe('test_lines.txt'))]) :-
%	write_lines('test_lines.txt', ["uno", "due", "tre"]),
%	read_lines('test_lines.txt', Lines),
%	assertion(Lines == ["uno", "due", "tre"]).

%test(delete_file_safe, [setup(delete_file_safe('test_lines.txt'))]) :-
%	write_lines('test_lines.txt', ["ciao"]),
%	delete_file_safe('test_lines.txt'),
%	\+ exists_file('test_lines.txt').

:- end_tests(file_utils).
