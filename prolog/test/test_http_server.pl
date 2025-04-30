:- begin_tests(http_server).
:- use_module(auxiliary/http_server).
:- use_module(utils/command_runner).

read_file_as_string(File, String) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_string(Stream, _, String),
        close(Stream)
    ).

download_file_to_string(URL, String) :-
	run_command("curl", ["-s", URL], False, False, String, _, ExitCode),
	ExitCode == 0.

%% Test: avvio di un Web server
%  Verifica che `start_http_server/2` sia in grado di
%  avviare un processo Web server tramite lo script
%  Python3 http_server.py e servire un file testuale.
test(start_http_server) :-
	Port = 80,
	Directory = "/home/kali/working_directory/prolog-dev/test",
	ServerURL = "http://localhost/",
	Filename = "file-read.txt",
	directory_file_path(Directory, Filename, FilePath),
	string_concat(ServerURL, Filename, FileURL),
	once(read_file_as_string(FilePath, FileContentLocal)),
	\+ running_http_server(Port, Directory),
	once(start_http_server(Port, Directory)),
	sleep(1),
	running_http_server(Port, Directory),
	once(download_file_to_string(FileURL, FileContentWeb)),
	assertion(FileContentLocal == FileContentWeb).

%% Test: ottenimento del PID di un Web server
%  Verifica che `http_server_pid/3` sia in grado di
%  ottenere il PID di un Web server a partire dalla
%  porta TCP e dalla directory radice.
test(http_server_pid) :-
	Port = 80,
	Directory = "/home/kali/working_directory/prolog-dev/test",
	running_http_server(Port, Directory),
        command_runner:running_detached(_, _, DesiredPID),
	http_server_pid(Port, Directory, PID),
	assertion(PID = DesiredPID).

%% Test: terminazione di un Web server
%  Verifica che `stop_http_server/1` sia in grado di
%  terminare un processo Web server tramite il
%  comando UNIX kill PID.
test(stop_http_server) :-
	Port = 80,
	running_http_server(Port, Directory),
	once(stop_http_server(Port)),
	assertion(\+ running_http_server(Port, Directory)).

:- end_tests(http_server).
