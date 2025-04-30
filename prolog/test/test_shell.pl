:- begin_tests(shell).
:- use_module(auxiliary/shell).
:- use_module(utils/command_runner).

%% Test: avvio di un reverse listener
%  Verifica che `start_reverse_listener/3` sia in grado di
%  avviare un processo reverse listener tramite lo script
%  Python3 reverse_listener.py.
test(start_reverse_listener) :-
	Port = 80,
	InputPipe = "/tmp/in",
	OutputPipe = "/tmp/out",
	\+ running_reverse_listener(Port, InputPipe, OutputPipe),
	once(start_reverse_listener(Port, InputPipe, OutputPipe)),
	running_reverse_listener(Port, InputPipe, OutputPipe).

%% Test: ottenimento del PID di un reverse listener
%  Verifica che `reverse_listener_pid/3` sia in grado di
%  ottenere il PID di un reverse listener a partire dalla
%  porta TCP e dai nomi delle named pipe in input e output.
test(reverse_listener_pid) :-
	Port = 80,
	InputPipe = "/tmp/in",
	OutputPipe = "/tmp/out",
	running_reverse_listener(Port, InputPipe, OutputPipe),
        command_runner:running_detached(_, _, DesiredPID),
	reverse_listener_pid(Port, InputPipe, OutputPipe, PID),
	assertion(PID = DesiredPID).

%% Test: terminazione di un reverse listener
%  Verifica che `stop_reverse_listener/1` sia in grado di
%  terminare un processo reverse listener tramite il
%  comando UNIX kill PID.
test(stop_reverse_listener) :-
	Port = 80,
	running_reverse_listener(Port, InputPipe, OutputPipe),
	once(stop_reverse_listener(Port)),
	assertion(\+ running_reverse_listener(Port, InputPipe, OutputPipe)).

:- end_tests(shell).

