/** <shell.pl> Utilità per la gestione di shell 
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per l'esecuzione di shell (per il momento, reverse
 *  listener).
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(shell, [
	start_reverse_listener/3,
	running_reverse_listener/3,
	reverse_listener_pid/4,
	stop_reverse_listener/1
]).

:- use_module(library(lists)).
:- use_module(utils/command_runner).

:- dynamic running_reverse_listener/3.
:- dynamic running_detached/3.

% Percorsi degli script
script_path("/home/kali/working_directory/prolog-dev/wrappers/reverse_listener.py").

%% start_reverse_listener(+Port:number, +InputPipe:string, +OutputPipe:string) is det
%
%  Il predicato start_reverse_listener/3 è un wrapper dello
%  script Python 3 reverse_listener.py che:
%  - apre una connessione in ascolto su una porta TCP
%  - crea due named pipe per l'input e l'output di comandi
%  Viene generato anche un predicato dinamico
%  running_reverse_listener/3 che tiene traccia dei
%  parametri del reverse listener.
%
%  @param Port La porta TCP su cui ascoltare.
%  @param InputPipe La named pipe da cui leggere l'input.
%  @param OutputPipe La named pipe da cui leggere l'output.
%                    comandi tramite sudo come utente root.
%
start_reverse_listener(Port, InputPipe, OutputPipe) :-
	running_reverse_listener(Port, InputPipe, OutputPipe), !.
start_reverse_listener(Port, InputPipe, OutputPipe) :-
	script_path(Path),
	Command = "python3",
	Args = [Path, "--port", Port, "--in", InputPipe, "--out", OutputPipe, "--cleanup-on-exit"],
	exec_detached(Command, Args),
	asserta(running_reverse_listener(Port, InputPipe, OutputPipe)).

%% reverse_listener_pid(+Port:number, +InputPipe:string, +OutputPipe:string, -PID:number) is det
%
%  Il predicato reverse_listener_pid/4 identifica il PID
%  del reverse listener a partire dalla porta e dalle due
%  named pipe in input e in output.
%
%  @param Port La porta TCP su cui ascoltare.
%  @param InputPipe La named pipe da cui leggere l'input.
%  @param OutputPipe La named pipe da cui leggere l'output.
%                    comandi tramite sudo come utente root.
%  @param PID Il PID del reverse listener in esecuzione.
%
reverse_listener_pid(Port, InputPipe, OutputPipe, PID) :-
	running_reverse_listener(Port, InputPipe, OutputPipe),
	script_path(Path),
	Cmd = "python3",
	Args = [Path, "--port", Port, "--in", InputPipe, "--out", OutputPipe, "--cleanup-on-exit"],
	command_runner:running_detached(Cmd, Args, PID).

%% stop_reverse_listener(+Port:number) is det
%
%  Il predicato stop_reverse_listener/1 termina
%  un reverse listener eseguito con lo script Python 3
%  reverse_listener.py.
%  Viene anche cancellato il predicato dinamico
%  running_reverse_listener/3 che tiene traccia dei
%  parametri del reverse listener.
%
%  @param Port La porta TCP su cui ascoltare.
%
stop_reverse_listener(Port) :-
	running_reverse_listener(Port, InputPipe, OutputPipe),
	reverse_listener_pid(Port, InputPipe, OutputPipe, PID),
	kill_detached(PID),
	retractall(running_reverse_listener(Port, InputPipe, OutputPipe)).
