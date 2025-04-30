/** <proxmox.pl> Utilità per la gestione delle VM su Proxmox
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per la gestione delle VM su Proxmox (start, stop,
 *  rollback).
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */
:- module(proxmox, [
	set_proxmox_credentials/2,
	proxmox_credentials/2,
	status_vm/4,
	start_vm/3,
	stop_vm/3,
      	rollback_vm/4
]).

:- use_module(utils/command_runner).

:- dynamic proxmox_credentials/2.

% Percorsi degli script
proxmox_script_path('/home/kali/working_directory/prolog-dev/wrappers/proxmox_control.py').

%% set_proxmox_credentials(+Username:string, +Password:string) is det 
%
%  Il predicato set_proxmox_credentials/2 crea un predicato
%  dinamico proxmox_credentials(Username, Password) che
%  memorizza le credenziali dell'utente Proxmox.
%
%  @param Username Lo username abilitato alla gestione
%                  delle VM tramite Proxmox.
%  @param Password La password dell'utente abilitato alla
%                  gestione delle VM tramite Proxmox.
%
set_proxmox_credentials(Username, Password) :-
	retractall(proxmox_credentials(_, _)),
	assertz(proxmox_credentials(Username, Password)).

%% status_vm(+Host:string, +Node:string, +VMID:int, -Status:string) is det
%
%  Il predicato status_vm/4 è un wrapper dello script
%  Python 3 proxmox_control.py che ritorna lo stato
%  di una VM su un server Proxmox ("running" o "stopped").
%  Il predicato richiede l'impostazione del predicato
%  proxmox_credentials/2 con le credenziali dell'utente
%  gestore di Proxmox.
%
%  @param Host L'host Proxmox.
%  @param Node Il nome del nodo Proxmox.
%  @param VMID L'ID della VM da avvviare.
%  @param Status La stringa ritornata da proxmox_control.py.
%
status_vm(Host, Node, VMID, Output) :-
	proxmox_credentials(Username, Password),
	proxmox_script_path(Script),
	run_command(
		"python3",
		[
			Script,
		       	"--no-verify",
			"--status",
			"--host", Host,
			"--user", Username,
			"--password", Password,
			"--node", Node,
			"--vmid", VMID
		],
		False,
		False,
		Stdout,
		_,
		ExitCode
	),
	ExitCode == 0,
	open_string(Stdout, Input),
	read_string(Input, "\n", "\n", _End, Output).

%
%  Il predicato start_vm/3 è un wrapper dello script
%  Python 3 proxmox_control.py che avvia una VM su
%  un server Proxmox.
%  Il predicato richiede l'impostazione del predicato
%  proxmox_credentials/2 con le credenziali dell'utente
%  gestore di Proxmox.
%
%  @param Host L'host Proxmox.
%  @param Node Il nome del nodo Proxmox.
%  @param VMID L'ID della VM da avvviare.
start_vm(Host, Node, VMID) :-
	proxmox_credentials(Username, Password),
	proxmox_script_path(Script),
	run_command(
		"python3",
		[
			Script,
		       	"--no-verify",
			"--start",
			"--host", Host,
			"--user", Username,
			"--password", Password,
			"--node", Node,
			"--vmid", VMID
		],
		False,
		False,
		_,
		_,
		ExitCode
	),
	ExitCode == 0.

%% stop_vm(+Host:string, +Node:string, +VMID:int) is det
%
%  Il predicato stop_vm/3 è un wrapper dello script
%  Python 3 proxmox_control.py che termina una VM su
%  un server Proxmox.
%  Il predicato richiede l'impostazione del predicato
%  proxmox_credentials/2 con le credenziali dell'utente
%  gestore di Proxmox.
%
%  @param Host L'host Proxmox.
%  @param Node Il nome del nodo Proxmox.
%  @param VMID L'ID della VM da avvviare.
stop_vm(Host, Node, VMID) :-
	proxmox_credentials(Username, Password),
	proxmox_script_path(Script),
	run_command(
		"python3",
		[
			Script,
		       	"--no-verify",
			"--stop",
			"--host", Host,
			"--user", Username,
			"--password", Password,
			"--node", Node,
			"--vmid", VMID
		],
		False,
		False,
		_,
		_,
		ExitCode
	),
	ExitCode == 0.

%% rollback_vm(+Host:string, +Node:string, +VMID:int, +Snapshot:string) is det
%
%  Il predicato rollback_vm/4 è un wrapper dello script
%  Python 3 proxmox_control.py che ripristina una VM
%  ad uno snapshot specifico su un server Proxmox.
%  Il predicato richiede l'impostazione del predicato
%  proxmox_credentials/2 con le credenziali dell'utente
%  gestore di Proxmox.
%
%  @param Host L'host Proxmox.
%  @param Node Il nome del nodo Proxmox.
%  @param VMID L'ID della VM da avvviare.
%  @param Snapshot Lo snapshot da ripristinare.
% Predicato per rollback di uno snapshot
rollback_vm(Host, Node, VMID, Snapshot) :-
	proxmox_credentials(Username, Password),
	proxmox_script_path(Script),
	run_command(
		"python3",
		[
			Script,
		       	"--no-verify",
			"--rollback",
			"--snapshot", Snapshot,
			"--host", Host,
			"--user", Username,
			"--password", Password,
			"--node", Node,
			"--vmid", VMID
		],
		False,
		False,
		_,
		_,
		ExitCode
	),
	ExitCode == 0.
