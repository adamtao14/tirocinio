
:- module(test_sql_injection_192_168_1_112, [test_sql_injection/0]).

:- use_module(library(process)).
:- use_module(library(readutil)).

% test_sql_injection/0
% Attempts SQL injection using sqlmap against the web form at http://192.168.1.112/.
% Assumes there is a form with two input fields (e.g., username & password).
% This module runs sqlmap with a generic POST payload.

test_sql_injection :-
    TargetURL = 'http://192.168.1.112/',
    % POST parameters example: user=admin&pass=test -- the field names might need to be adjusted
    Data = 'user=admin&pass=test',
    Command = [
        sqlmap,
        '-u', TargetURL,
        '--data', Data,
        '--batch',         % non-interactive
        '--level', '2',
        '--risk', '2',
        '--output-dir', '/tmp/sqlmap_output'
    ],
    format("[-] Running sqlmap against ~w with data: ~w~n", [TargetURL, Data]),
    process_create(path(sqlmap), Command, [stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]),
    read_stream_to_codes(Out, StdoutCodes),
    read_stream_to_codes(Err, StderrCodes),
    close(Out), close(Err),
    wait(PID, _Status),
    string_codes(Stdout, StdoutCodes),
    string_codes(Stderr, StderrCodes),
    (
        contains_vulnerability(Stdout)
    ->  format("[+] SQL injection vulnerability found on ~w~n", [TargetURL])
    ;   format("[-] No SQL injection vulnerability detected on ~w~n", [TargetURL])
    ),
    % Optionally, output some context or important lines
    format("sqlmap output (truncated):~n"),
    print_sqlmap_relevant_output(Stdout),
    fail. % always fail to prevent accidental reevaluation in chaining (remove if undesired)
test_sql_injection.

% contains_vulnerability(+SqlmapOutput)
% Looks for key indicators from sqlmap reports
contains_vulnerability(Output) :-
    % Typical sqlmap indicators
    sub_string(Output, _, _, _, "is vulnerable"),
    !.
contains_vulnerability(Output) :-
    sub_string(Output, _, _, _, "Parameter:"),
    sub_string(Output, _, _, _, "Type:"),
    !.
contains_vulnerability(Output) :-
    sub_string(Output, _, _, _, "[CRITICAL]"),
    sub_string(Output, _, _, _, "sql injection"),
    !.

% print_sqlmap_relevant_output(+FullOutput)
print_sqlmap_relevant_output(Output) :-
    split_string(Output, "\n", "\n", Lines),
    include(sqlmap_interesting_line, Lines, Interesting),
    forall(member(Line, Interesting),
           format("~s~n", [Line])).

% sqlmap_interesting_line(+Line)
% Filters for summary and result lines
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "is vulnerable"), !.
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "Parameter:"), !.
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "Type:"), !.
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "sql injection"), !.
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "[INFO]"), !.
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "[CRITICAL]"), !.
sqlmap_interesting_line(Line) :-
    sub_string(Line, _, _, _, "[WARNING]"), !.

% NOTE: You may need to change 'user' and 'pass' to the actual field names for the form.
% This module assumes sqlmap is in PATH.
