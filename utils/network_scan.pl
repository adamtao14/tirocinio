:- module(network_scanner, [scan_network/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).

scan_network(Subnet, ActiveIPs) :-
    format(string(Cmd), "nmap -sP -oG - ~w", [Subnet]),
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    string_codes(OutputStr, Codes),
    extract_grepable_ips(OutputStr, ActiveIPs).

% Extract IPs from grepable format
extract_grepable_ips(Output, IPs) :-
    split_string(Output, "\n", "", Lines),
    include(is_host_up_line, Lines, HostLines),
    maplist(extract_ip_from_line, HostLines, IPs).

% Filter for active hosts
is_host_up_line(Line) :-
    sub_string(Line, _, _, _, "Status: Up").

% Improved IP extraction without singleton variables
extract_ip_from_line(Line, IP) :-
    sub_string(Line, Start, _, _, "Host: "),
    IPStart is Start + 6,
    sub_string(Line, IPStart, 15, _, IPPart),  % Max length of IP+padding
    split_string(IPPart, " \t(", "", [IP|_]),  % Split on space, tab or (
    validate_ip(IP).

% IP validation helper
validate_ip(IP) :-
    split_string(IP, ".", "", Parts),
    length(Parts, 4),
    maplist(number_string, [A,B,C,D], Parts),
    maplist(between(0, 255), [A,B,C,D]).