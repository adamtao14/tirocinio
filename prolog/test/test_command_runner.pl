:- begin_tests(command_runner).
:- use_module(utils/command_runner).

%% Test: produzione di una linea di caratteri
%  Verifica che `length_line/3` sia in grado di produrre
%  correttamente una riga di caratteri.
test(length_line) :-
	Char = '=',
	N = 20,
	DesiredLine = "====================",
	once(command_runner:length_line(Char, N, Line)),
	assertion(Line == DesiredLine).

%% Test: produzione di un blocco di output delimitato
%  Verifica che `format_block_output/2` sia in grado di
%   produrre un blocco di output delimitato da linee
%   costruite con il caratter "=".
test(format_output_block) :-
	Title = "Titolo",
	Content = "Riga1\nRiga2",
	DesiredBlock = "========== Titolo ==========\nRiga1\nRiga2\n============================\n",
	once(format_output_block(Title, Content, Block)),
	assertion(Block == DesiredBlock).

%% Test: conversione di una lista di argomenti in stringa
%  Verifica che `args_to_string/2` converta la lista nulla
%  nella stringa nulla.
test(empty_args_to_string) :-
	List = [],
	DesiredString = "",
	once(command_runner:args_to_string(List, String)),
	assertion(String == DesiredString).

%% Test: conversione di una lista di argomenti in stringa
%  Verifica che `args_to_string/2` converta una lista di
%  un unico elemento in un'unica stringa.
test(single_args_to_string) :-
	List = ["Arg1"],
	DesiredString = "Arg1",
	once(command_runner:args_to_string(List, String)),
	assertion(String == DesiredString).

%% Test: conversione di una lista di argomenti in stringa
%  Verifica che `args_to_string/2` converta una lista di
%  più elementi in un'unica stringa di elementi separati
%  da " ".
test(args_to_string) :-
	List = ["Arg1", "Arg2", "Arg3"],
	DesiredString = "Arg1 Arg2 Arg3",
	once(command_runner:args_to_string(List, String)),
	assertion(String == DesiredString).

%% Test: impostazione delle credenziali utente per sudo 
%  Verifica che `set_sudo_credentials/2` sia in grado di
%  creare un predicato dinamico sudo_credentials(Username,
%  Password) contenente le credenziali dell'utente
%  abilitato ad eseguire comandi come root tramite sudo.
%  Alla fine del test si cancella il predicato dinamico
%  creato per non sporcare l'ambiente.
test(set_sudo_credentials) :-
	Username = "kali",
	Password = "kali",
	once(set_sudo_credentials(Username, Password)),
	once(command_runner:sudo_credentials(DesiredUsername, DesiredPassword)),
	assertion(Username == DesiredUsername),
	assertion(Password == DesiredPassword),
	retractall(sudo_credentials(_, _)).

%% Test: individuazione di un processo inesistente
%  Verifica che `is_pid_alive/1` ritorni false in caso
%  di PID inesistente.
test(is_pid_alive_nonexisting_process) :-
	PID = 1111111111,
	\+ is_pid_alive(PID).

%% Test: individuazione di un processo esistente
%  Verifica che `is_pid_alive/3` ritorni true in caso
%  di PID esistente.
test(is_pid_alive_existing_process) :-
	PID = 1,
	is_pid_alive(PID).

%% Test: esecuzione di un comando invalido
%  Verifica che `run_command/7` lanci un'eccezione se il
%  comando non esiste.
%  Deve uscire con un messaggio di errore su standard 
%  error contenente la stringa "Could not find executable",
%  standard output nullo e codice di uscita 127.
test(run_invalid_command_throws_exception) :-
	Cmd = "nonexistent",
	Args = ["-nonexistent", "nonexistent"],
	AsRoot = false,
	Detached = false,
	catch(
		run_command(Cmd, Args, AsRoot, Detached, Stdout, Stderr, ExitCode),
		Error,
		(
			message_to_string(Error, ErrorStr),
			assertion(sub_string(ErrorStr, _, _, _, "Could not find executable")),
			Stdout = "",	  % Comportamento atteso
			Stderr = ErrorStr,
			ExitCode = 127
		)
	).

%% Test: esecuzione di un comando valido come "kali"
%  Verifica che `run_command/7` sia in grado di eseguire
%  un comando valido con privilegi utente normali.
%  Deve uscire con ExitCode 0, Stdout esatto e Stderr
%  nullo.
test(run_valid_command_as_kali) :-
	Cmd = "id",
	Args = ["-u", "kali"],
	AsRoot = false,
	Detached = false,
	DesiredStdout = "1000\n",
	DesiredStderr = "",
	DesiredExitCode = 0,
	once(run_command(Cmd, Args, AsRoot, Detached, Stdout, Stderr, ExitCode)),
	assertion(Stdout == DesiredStdout),
	assertion(Stderr == DesiredStderr),
	assertion(ExitCode == DesiredExitCode).

%% Test: esecuzione di un comando valido come "root"
%  Verifica che `run_command/7` ritorni false se si
%  prova ad eseguire un comando valido come "root"
%  senza aver impostato le credenziali dell'utente
%  sudoer tramite set_sudo_credentials/2.
test(run_valid_command_as_root_no_creds) :-
	Cmd = "id",
	Args = ["-u", "root"],
	AsRoot = true,
	Detached = false,
	\+ run_command(Cmd, Args, AsRoot, Detached, _, _, _).

%% Test: esecuzione di un comando valido come "root"
%  Verifica che `run_command/7` sia in grado di eseguire
%  un comando valido con privilegi di root se sono state
%  impostate le credenziali dell'utente sudoer con il
%  predicato set_sudo_credentials/2.
%  Deve uscire con ExitCode 0, Stdout esatto e Stderr
%  nullo o contenente "sudo] password for kali: ".
test(run_valid_command_as_root) :-
	Username = "kali",
	Password = "kali",
	set_sudo_credentials(Username, Password),
	Cmd = "id",
	Args = ["-u", "root"],
	AsRoot = true,
	Detached = false,
	DesiredStdout = "0\n",
	DesiredStderrEmpty = "",
	DesiredStderrSudo = "[sudo] password for kali: ",
	DesiredExitCode = 0,
	once(run_command(Cmd, Args, AsRoot, Detached, Stdout, Stderr, ExitCode)),
	writeln(Stdout),
	writeln(Stderr),
	writeln(ExitCode),
	assertion(Stdout == DesiredStdout),
	once(
		assertion(
			Stderr == DesiredStderrEmpty ; Stderr == DesiredStderrSudo
		)
	),
	assertion(ExitCode == DesiredExitCode),
	retractall(sudo_credentials(_, _)).

%% Test: esecuzione di un comando valido come "root"
%  Verifica che `exec_as_root/7` sia in grado di impostare
%  le credenziali dell'utente sudoer con il predicato
%  set_sudo_credential/2 ed eseguire un comando valido
%  con privilegi di root.
%  Deve uscire con ExitCode 0, Stdout esatto e Stderr
%  nullo o contenente "[sudo] password for kali: ".
test(exec_as_root) :-
	Username = "kali",
	Password = "kali",
	Cmd = "id",
	Args = ["-u", "root"],
	DesiredStdout = "0\n",
	DesiredStderrEmpty = "",
	DesiredStderrSudo = "[sudo] password for kali: ",
	DesiredExitCode = 0,
	once(exec_as_root(Cmd, Args, Username, Password, Stdout, Stderr, ExitCode)),
	assertion(Stdout == DesiredStdout),
	once(
		assertion(
			Stderr == DesiredStderrEmpty ; Stderr == DesiredStderrSudo
		)
	),
	assertion(ExitCode == DesiredExitCode),
	retractall(sudo_credentials(_, _)).

%% Test: esecuzione di un comando detached
%  Verifica che `run_command/7` sia in grado di eseguire
%  un comando valido in background e crei il predicato
%  dinamico running_detached(Cmd, Args, PID) che memorizza
%  comando e PID del processo.
%  Si controlla l'esistenza del processo tramite il
%  suo PID e lo si uccide. 
%  Alla fine del test si cancella il predicato dinamico
%  creato per non sporcare l'ambiente.
test(run_valid_detached_command_as_kali) :-
	Cmd = "sleep",
	Args = ["600"],
	AsRoot = false,
	Detached = true,
	once(run_command(Cmd, Args, AsRoot, Detached, _, _, _)),
	once(command_runner:running_detached(Cmd, Args, PID)),
	is_pid_alive(PID),
	process_kill(PID),
	% il predicato running_detached/3 è eseguito dal
	% modulo command_runner, pertanto è visibile
	% solo nel suo namespace!
	retractall(command_runner:running_detached(Cmd, Args, PID)).

%% Test: esecuzione di un comando detached
%  Verifica che `exec_detached/2` sia in grado di eseguire
%  un comando valido in background, scartando standard
%  output, standard error e stato di uscita.
%  Si controlla l'esistenza del processo tramite il
%  suo PID e lo si uccide. 
%  Alla fine del test si cancella il predicato dinamico
%  creato per non sporcare l'ambiente.
test(exec_detached) :-
	Cmd = "sleep",
	Args = ["600"],
	once(exec_detached(Cmd, Args)),
	once(command_runner:running_detached(Cmd, Args, PID)),
	is_pid_alive(PID),
	process_kill(PID),
	retractall(command_runner:running_detached(Cmd, Args, PID)).

%% Test: esecuzione di un comando detached come root
%  Verifica che `exec_detached_as_root/7` sia in grado di eseguire
%  un comando valido in background con i privilegi di root
%  e crei il predicato dinamico running_detached(Cmd, Args,
%  PID) che memorizza comando, argomenti e PID del
%  processo.
%  Si controlla l'esistenza del processo tramite il suo PID
%  e lo si uccide. 
%  Alla fine del test si cancella il predicato dinamico
%  creato per non sporcare l'ambiente.
test(run_valid_detached_command_as_root) :-
	Username = "kali",
	Password = "kali",
	set_sudo_credentials(Username, Password),
	Cmd = "sleep",
	Args = ["600"],
	AsRoot = true,
	Detached = true,
	once(run_command(Cmd, Args, AsRoot, Detached, _, _, _)),
	once(command_runner:running_detached(Cmd, Args, PID)),
	assertion(is_pid_alive(PID)),
	once(exec_as_root("kill", [PID], Username, Password, _, _, _)),
	retractall(command_runner:running_detached(Cmd, Args, PID)).

%% Test: uccisione di un processo detached
%  Verifica che `kill_detached/1` sia in grado di uccidere
%  un processo detached in esecuzione con privilegi normali
%  tramite il comando UNIX kill PID, eseguito con i
%  privilegi di kali. Inoltre, verifica che il predicato
%  dinamico running_detached/3 non sia più presente.
test(kill_detached_as_kali) :-
	Cmd = "sleep",
	Args = ["600"],
	once(run_command(Cmd, Args, false,  true, _, _, _)),
	once(command_runner:running_detached(Cmd, Args, PID)),
	assertion(kill_detached(PID)),
	assertion(\+ is_pid_alive(PID)),
	assertion(\+ command_runner:running_detached(Cmd, Args, PID)).

%% Test: uccisione di un processo detached
%  Verifica che `kill_detached_as_root/3` sia in grado di
%  uccidere un processo detached in esecuzione con
%  privilegi di root tramite il comando UNIX kill PID,
%  eseguito con i privilegi di root. Inoltre, verifica
%  che il predicato dinamico running_detached/3 non sia più
%  presente.
test(kill_detached_as_root) :-
	Cmd = "sleep",
	Args = ["600"],
	Username = "kali",
	Password = "kali",
	once(exec_detached_as_root(Cmd, Args, Username, Password)),
	once(command_runner:running_detached(Cmd, Args, PID)),
	assertion(kill_detached_as_root(PID, Username, Password)),
	assertion(\+ is_pid_alive(PID)),
	assertion(\+ command_runner:running_detached(Cmd, Args, PID)).

:- end_tests(command_runner).
