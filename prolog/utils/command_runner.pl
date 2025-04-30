/** <command_runner> Utilità per l'esecuzione di comandi
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per l'esecuzione di comandi UNIX.
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(command_runner, [
	format_output_block/3,
	is_pid_alive/1,
	run_command/7,
	set_sudo_credentials/2,
	sudo_credentials/2,
	exec_as_root/7,
	exec_detached/2,
	exec_detached_as_root/4,
	kill_detached/1,
	kill_detached_as_root/3
]).

:- use_module(library(readutil)).
:- use_module(library(process)).
:- dynamic sudo_credentials/2.
:- dynamic running_detached/3.


%% length_line(+Char:string, +N:int, -Line:string) is det
%
%  Il predicato length_line/3 produce una stringa di
%  N caratteri Char e la memorizza in Line.
%
%  @param Char Il carattere usato per generare la linea.
%  @param N Il numero di caratteri da generare.
%  @param Line La stringa prodotta in output.
%
length_line(Char, N, Line) :-
	length(Codes, N),
	maplist(=(Char), Chars),
	maplist(char_code, Chars, Codes),
	string_codes(Line, Codes).

%% format_output_block(+Title:string, +Content:string, -Block:string) is det
%
%  Il predicato format_output_block/2 produce una
%  stringa contenente un blocco di output separato
%  da due righe e la memorizza in Block.
%  Nella riga superiore compare il titolo del blocco.
%
%  @param Title Il titolo del blocco da stampare.
%  @param Content Il contenuto da stampare. 
%  @param Block La stringa prodotta in output.
%
format_output_block(Title, Content, Block) :-
	format(string(Header), "========== ~w ==========", [Title]),
	string_length(Header, Len),
	length_line('=', Len, Footer),
	format(string(Body), "~w", [Content]),
	format(string(Block), "~s\n~s\n~s\n", [Header, Body, Footer]).

%% args_to_string(+Args:list, -String:string) is nondet
%
%  Il predicato args_to_string/2 traduce una lista di
%  argomenti in una stringa di argomenti separata da spazi,
%  memorizzandola in String.
%
%  @param List La lista degli argomenti.
%  @param String La stringa prodotta in output.
%
args_to_string([], "").
args_to_string([H], S) :-
	format(string(S), "~w", [H]).
args_to_string([H|T], S) :-
	args_to_string(T, Rest),
	format(string(S), "~w ~s", [H, Rest]).

%% set_sudo_credentials(+Username:string, +Password:string) is det 
%
%  Il predicato set_sudo_credentials/2 crea un predicato
%  dinamico sudo_credentials(Username, Password) che memorizza
%  le credenziali di utente con sudo abilitato.
%
%  @param Username Lo username abilitato all'esecuzione di
%                  comandi tramite sudo come utente root.
%  @param Password La password dell'utente abilitato alla
%                  esecuzione di sudo.
%
set_sudo_credentials(Username, Password) :-
	retractall(sudo_credentials(_, _)),
	assertz(sudo_credentials(Username, Password)).


%% is_pid_alive(+PID:int) is semidet
%
%  Il predicato is_pid_alive/2 verifica se un processo con
%  dato PID è ancora in esecuzione. Se lo è, ritorna true,
%  altrimenti ritorna false.
%  La tecnica usata per verificare l'esistenza del processo
%  è l'esecuzione del comando UNIX kill -0 PID con i
%  privilegi di root. Questo comando ritorna 0 se il
%  processo esiste, 1 altrimenti.
%
%  @param Cmd Il comando eseguito.
%  @param Args La lista con gli argomenti del comando.
%  @param PID Il PID del processo più recente.
%
is_pid_alive(PID) :-
	number(PID),
	set_sudo_credentials("kali", "kali"),
	sudo_credentials(_, Password),
	format(atom(ShellCmd), "echo '~w' | sudo -S kill -0 ~w", [Password, PID]),
	process_create(path(sh), ['-c', ShellCmd], [process(ShellCmdPID)]),
	process_wait(ShellCmdPID, exit(ExitCode)),
	retractall(sudo_credentials("kali", "kali")),
	ExitCode = 0.

%% run_command(+Cmd:string, +Args:list, +AsRoot:bool, +Detached:bool, -Stdout:string, -Stderr:string, -ExitCode:int) is det
%
%  Il predicato run_command/7 esegue un comando UNIX.
%  Il comando può essere eseguito normalmente, in
%  background, con i privilegi di root tramite il
%  comando sudo. Se è eseguito in background viene
%  generato un predicato dinamico running_detached/3
%  che consente di ricordarsene in futuro.
%  Il predicato può sollevare eccezioni se il comando
%  è invalido.
%
%  @param Cmd Il nome o il percorso del comando da eseguire.
%  @param Args La lista degli argomenti del comando.
%  @param AsRoot Se true, il comando è eseguito con i
%                privilegi di root tramite il comando sudo.
%                Richiede la definizione del predicato
%                sudo_credentials().
%  @param Detached Se true, il comando è eseguito i)n
%                  modalità background. È utile per
%                  demoni UNIX e processi lunghi.
%  @param Stdout Se il comando non è eseguito in
%                background, contiene lo standard output
%                del comando eseguito.
%                Altrimenti, è la stringa vuota.
%  @param Stderr Se il comando non è eseguito in
%                background, contiene lo standard error
%                del comando eseguito.
%                Altrimenti, è la stringa vuota.
%  @param ExitCode Contiene il codice di uscita del
%                  comando eseguito.
%
%  @throws error(existence_error(process, Cmd), _)
%
run_command(Cmd, Args, AsRoot, Detached, Stdout, Stderr, ExitCode) :-
	args_to_string(Args, ArgStr),
	( AsRoot == true ->
		sudo_credentials(_, Password),
		( Detached == true ->
			format(atom(ShellCmd), "echo '~w' | sudo -S sh -c '~w ~w & echo $!'", [Password, Cmd, ArgStr]),
			process_create(path(sh), ['-c', ShellCmd], [stdout(pipe(Out))]),
			read_line_to_string(Out, PidStr),
			close(Out),
			normalize_space(atom(PidAtom), PidStr),
			atom_number(PidAtom, PID),
			assertz(running_detached(Cmd, Args, PID)),
			copy_term("", Stdout),
			copy_term("", Stderr),
			ExitCode = 0
		;
			format(atom(FullCmd), "echo '~w' | sudo -S ~w ~w", [Password, Cmd, ArgStr]),
			process_create(path(sh), ['-c', FullCmd],
				[stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]),
			read_string(Out, _, Stdout),
			read_string(Err, _, Stderr),
			close(Out), close(Err),
			process_wait(PID, exit(ExitCode))
		)
	;
		( Detached == true ->
			format(atom(ShellCmd), "~w ~w & echo $!", [Cmd, ArgStr]),
			process_create(path(sh), ['-c', ShellCmd], [stdout(pipe(Out))]),
			read_line_to_string(Out, PidStr),
			close(Out),
			normalize_space(atom(PidAtom), PidStr),
			atom_number(PidAtom, PID),
			assertz(running_detached(Cmd, Args, PID)),
			copy_term("", Stdout),
			copy_term("", Stderr),
			ExitCode = 0
		;
			process_create(path(Cmd), Args,
				[stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]),
			read_string(Out, _, Stdout),
			read_string(Err, _, Stderr),
			close(Out), close(Err),
			process_wait(PID, exit(ExitCode))
		)
	).

%% exec_as_root(+Cmd:string, +Args:list, +Username:string, +Password:string, -Stdout:string, -Stderr:string, -ExitCode:int) is det
%
%  Il predicato exec_as_root/7 è un wrapper del predicato
%  run_command/7 che imposta le credenziali dell'utente
%  sudoer tramite il predicato set_sudo_credential/2 ed
%  esegue un comando UNIX con i privilegi di root.
%  L'esecuzione non è in background.
%  Il predicato può sollevare eccezioni se il comando
%  è invalido.
%
%  @param Cmd Il nome o il percorso del comando da eseguire.
%  @param Args La lista degli argomenti del comando.
%  @param Username Lo username abilitato all'esecuzione di
%                  comandi tramite sudo come utente root.
%  @param Password La password dell'utente abilitato alla
%                  esecuzione di sudo.
%  @param Stdout Contiene lo standard output del comando
%                eseguito.
%  @param Stderr Contiene lo standard error del comando
%                eseguito.
%  @param ExitCode Contiene il codice di uscita del
%                  comando eseguito.
%
%  @throws error(existence_error(process, Cmd), _)
exec_as_root(Cmd, Args, Username, Password, Stdout, Stderr, ExitCode) :-
	set_sudo_credentials(Username, Password),
	run_command(Cmd, Args, true, false, Stdout, Stderr, ExitCode).

%% exec_detached(+Cmd:string, +Args:list) is det
%
%  Il predicato exec_as_root/7 è un wrapper del predicato
%  run_command/7 che esegue un comando UNIX in background.
%  Questo predicato ha senso solo per comandi di lunga
%  esecuzione (demoni UNIX in primis). Non sono catturati
%  standard output, standard error e codice di uscita.
%  Il predicato può sollevare eccezioni se il comando
%  è invalido.
%
%  @param Cmd Il nome o il percorso del comando da eseguire.
%  @param Args La lista degli argomenti del comando.
%  @param Stdout Contiene lo standard output del comando
%                eseguito.
%  @param Stderr Contiene lo standard error del comando
%                eseguito.
%  @param ExitCode Contiene il codice di uscita del
%                  comando eseguito.
%
%  @throws error(existence_error(process, Cmd), _)
%
exec_detached(Cmd, Args) :-
	run_command(Cmd, Args, false, true, _, _, _).

%% exec_detached_as_root(+Cmd:string, +Args:list, +Username:string, +Password:string) is det
%
%  Il predicato exec_as_root/7 è un wrapper del predicato
%  run_command/7 che imposta le credenziali dell'utente
%  sudoer tramite il predicato set_sudo_credential/2 ed
%  esegue un comando UNIX in background.
%  Questo predicato ha senso solo per comandi di lunga
%  esecuzione (demoni UNIX in primis). Non sono catturati
%  standard output, standard error e codice di uscita.
%  Il predicato può sollevare eccezioni se il comando
%  è invalido.
%
%  @param Cmd Il nome o il percorso del comando da eseguire.
%  @param Args La lista degli argomenti del comando.
%  @param Username Lo username abilitato all'esecuzione di
%                  comandi tramite sudo come utente root.
%  @param Password La password dell'utente abilitato alla
%                  esecuzione di sudo.
%
exec_detached_as_root(Cmd, Args, Username, Password) :-
	set_sudo_credentials(Username, Password),
	run_command(Cmd, Args, true, true, _, _, _).

%% kill_detached(+PID:int) is det
%
%  Il predicato kill_detached/1 uccide un processo detached
%  caratterizzato dal suo PID, a patto che sia
%  effettivamente in esecuzione. Se il processo è in
%  esecuzione si ritorna true, altrimenti false.
%  Il controllo di esecuzione è svolto tramite il predicato
%  is_pid_alive/1. L'uccisione avviene tramite l'esecuzione
%  del comando UNIX kill PID.
%  Infine viene rimosso il predicato dinamico
%  running_detached(_, _, PID) corrispondente.
%  Questo predicato ha senso solo per comandi di lunga
%  esecuzione (demoni UNIX in primis).
%
%  @param PID Il PID del processo detached in esecuzione.
%
kill_detached(PID) :-
	is_pid_alive(PID),
	KillCmd = "kill",
	KillArgs = [PID],
	run_command(KillCmd, KillArgs, _, _, _, _, _),
	retractall(running_detached(_, _, PID)).

%% kill_detached_as_root(+PID:int, +Username:string, +Password:string) is det
%
%  Il predicato kill_detached_as_root/3 uccide un processo
%  detached caratterizzato da Cmd, Args e PID, a patto che
%  sia effettivamente in esecuzione. Se il processo è in
%  esecuzione si ritorna true, altrimenti false.
%  Il controllo di esecuzione è svolto tramite il predicato
%  is_pid_alive/1. L'uccisione avviene tramite l'esecuzione
%  del comando UNIX kill PID con i privilegi di root
%  tramite l'utente sudoer impostato con il predicato
%  set_sudo_credentials/2.
%  Infine viene rimosso il predicato dinamico
%  running_detached(Cmd, Args, PID) corrispondente.
%  Questo predicato ha senso solo per comandi di lunga
%  esecuzione (demoni UNIX in primis).
%
%  @param PID Il PID del processo detached in esecuzione.
%  @param Username Lo username abilitato all'esecuzione di
%                  comandi tramite sudo come utente root.
%  @param Password La password dell'utente abilitato alla
%                  esecuzione di sudo.
%
kill_detached_as_root(PID, Username, Password) :-
	is_pid_alive(PID),
	set_sudo_credentials(Username, Password),
	sudo_credentials(_, Password),
	KillCmd = "kill",
	KillArgs = [PID],
	exec_as_root(KillCmd, KillArgs, Username, Password, _, _, _),
	retractall(running_detached(_, _, PID)).
