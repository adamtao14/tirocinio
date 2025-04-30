/** <smb_shell> Utilità per l'esecuzione di comandi tramite SMB
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per l'esecuzione di comandi tramite protocllo SMB,
 *  usando lo script psexec.py di Impacket.
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */
:- module(smb_shell, [
	run_command_smb/8
]).

:- use_module(utils/command_runner).

% Percorsi degli script
script_path("/home/kali/working_directory/utils/psexec.py").

%% run_command_smb(+Cmd:string, +Args:list, +Host:string, +Username:string, +Password:string, -Stdout:string, -Stderr:string, -ExitCode:int) is det
%
%  Il predicato run_command_smb/7 esegue un comando
%  Windows tramite protocollo SMB, usando lo script
%  psexec.py di Impacket.
%
%  @param Cmd Il nome o il percorso del comando da eseguire.
%  @param Args La lista degli argomenti del comando.
%  @param Host L'host esponente il servizio SMB su cui
%              testare la password.
%  @param Username Lo username dell'utente che ha il
%                  privilegio di uso di SMB.
%  @param Password La password dell'utente che ha il
%                  privilegio di uso di SMB.
%  @param Stdout Contiene lo standard output del comando
%                eseguito.
%  @param Stderr Contiene lo standard error del comando
%                eseguito.
%  @param ExitCode Contiene il codice di uscita del
%                  comando eseguito.
%
run_command_smb(Cmd, Args, Host, Username, Password, Stdout, Stderr, ExitCode) :-
	script_path(Script),
	string_concat(Username, ":", Part1),
	string_concat(Part1, Password, Part2),
	string_concat(Part2, "@", Part3),
	string_concat(Part3, Host, ConnectString),
	atomic_list_concat(Args, ' ', ArgsString),
	run_command(
		"python3",
		[
			Script,
			ConnectString,
			Cmd,
			ArgsString
		],
		false,
		false,
		Stdout,
		Stderr,
		ExitCode).
