:- module(command_utils, [
    run_command/4,
    run_command_as/5,
    run_elevated_command/4
]).

:- use_module(library(process)).
:- use_module(library(readutil)).

:- dynamic password_file/1.

operator_username('kali').
operator_password('kali').

% run_command(+Command, +Args, -Result, -stdout_stderr(Stdout, Stderr))
% Esegue un comando con argomenti, cattura stdout e stderr, restituisce true se exit status = 0
run_command(Command, Args, Result, stdout_stderr(Stdout, Stderr)) :-
    process_create(path(Command), Args,
        [ stdout(pipe(Out)),
          stderr(pipe(Err)),
          process(PID)
        ]),
    read_stream_to_codes(Out, OutCodes),
    read_stream_to_codes(Err, ErrCodes),
    close(Out),
    close(Err),
    process_wait(PID, exit(Status)),
    ( Status =:= 0 -> Result = true ; Result = false ),
    string_codes(Stdout, OutCodes),
    string_codes(Stderr, ErrCodes).

% run_command_as(+Command, +Args, OperatorPassword, -Result, -stdout_stderr(Stdout, Stderr))
run_command_as(Command, Args, Password, Result, stdout_stderr(Stdout, Stderr)) :-
    atomic_list_concat(Args, ' ', ArgsAtom),
    atomic_list_concat([Command, ArgsAtom], ' ', FullCommand),
    format(atom(ShellCommand), "echo ~w | sudo -S ~w", [Password, FullCommand]),
    process_create(path(sh), ['-c', ShellCommand],
        [ stdout(pipe(Out)),
          stderr(pipe(Err)),
          process(PID)
        ]),
    read_stream_to_codes(Out, OutCodes),
    read_stream_to_codes(Err, ErrCodes),
    close(Out),
    close(Err),
    process_wait(PID, exit(Status)),
    ( Status =:= 0 -> Result = true ; Result = false ),
    string_codes(Stdout, OutCodes),
    string_codes(Stderr, ErrCodes).

% run_elevated_command(+Command, +Args, -Result, -stdout_stderr(Stdout, Stderr))
run_elevated_command(Command, Args, Result, stdout_stderr(Stdout, Stderr)) :-
    operator_password(Password),
    run_command_as(Command, Args, Password, Result, stdout_stderr(Stdout, Stderr)).
