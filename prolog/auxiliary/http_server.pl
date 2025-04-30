/** <http_server.pl> Utilità per la gestione di un Web server 
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per la gestione di un Web server (con l'obiettivo di
 *  trasferire file su una macchina remota).
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(http_server, [
	start_http_server/2,
	running_http_server/2,
	http_server_pid/3,
	stop_http_server/1
]).

:- use_module(library(lists)).
:- use_module(utils/command_runner).

:- dynamic running_http_server/2.
:- dynamic running_detached/3.

% Percorsi degli script
script_path("/home/kali/working_directory/prolog-dev/wrappers/http_server.py").

%% start_http_server(+Port:number, +Directory) is det
%
%  Il predicato start_http_server/2 è un wrapper dello
%  script Python 3 http_server.py che:
%  - esegue un Web server su una porta TCP
%  - serve i documenti da una specifica directory
%  Viene generato anche un predicato dinamico
%  running_http_server/2 che tiene traccia dei
%  parametri del Web server.
%
%  @param Port La porta TCP su cui ascoltare.
%  @param Directory La directory radice del Web server.
%
start_http_server(Port, Directory) :-
	running_http_server(Port, Directory), !.
start_http_server(Port, Directory) :-
	script_path(Path),
	Command = "python3",
	Args = [Path, "--port", Port, "--directory", Directory],
	exec_detached(Command, Args),
	asserta(running_http_server(Port, Directory)).

%% http_server_pid(+Port:number, +Directory:string, -PID:number) is det
%
%  Il predicato http_server_pid/3 identifica il PID
%  del Web server a partire dalla porta e dalla
%  directory radice.
%
%  @param Port La porta TCP su cui ascoltare.
%  @param Directory La directory radice del Web server.
%  @param PID Il PID del Web server in esecuzione.
%
http_server_pid(Port, Directory, PID) :-
	running_http_server(Port, Directory),
	script_path(Path),
	Cmd = "python3",
	Args = [Path, "--port", Port, "--directory", Directory],
	command_runner:running_detached(Cmd, Args, PID).

%% stop_http_server(+Port:number) is det
%
%  Il predicato stop_http_server/1 termina un Web server
%  eseguito con lo script Python 3 http_server.py.
%  Viene anche cancellato il predicato dinamico
%  running_http_server/2 che tiene traccia dei
%  parametri del Web server.
%
%  @param Port La porta TCP su cui ascolta il processo
%              Web server in esecuzione%
%
stop_http_server(Port) :-
	running_http_server(Port, Directory),
	http_server_pid(Port, Directory, PID),
	kill_detached(PID),
	retractall(running_http_server(Port, Directory)).
