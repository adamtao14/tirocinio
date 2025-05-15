:- module(network_scanner, [scan_network/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).

scan_network(Subnet, ActiveIPs) :-
    format(string(Cmd), "nmap -sP -oG - ~w", [Subnet]),  % -oG - outputs grepable format to stdout
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    string_codes(OutputStr, Codes),
    extract_grepable_ips(OutputStr, ActiveIPs).

% Extract IPs from grepable format output
extract_grepable_ips(Output, IPs) :-
    split_string(Output, "\n", "", Lines),
    include(is_host_up_line, Lines, HostLines),
    maplist(extract_ip_from_line, HostLines, IPs).

% Filter for lines containing "Status: Up"
is_host_up_line(Line) :-
    sub_string(Line, _, _, _, "Status: Up").

% Extract IP from grepable format line (Host: 192.168.1.1 (...) Status: Up)
extract_ip_from_line(Line, IP) :-
    sub_string(Line, Start, _, _, "Host: "),
    sub_string(Line, Start+6, Len, _, Rest),
    sub_string(Rest, 0, IPLen, _, IP),
    (   sub_string(IP, _, _, 1, " ")  % Handle cases with hostnames
    ->  sub_string(IP, 0, _, 1, CleanIP)  % Remove trailing space
    ;   CleanIP = IP
    ),
    validate_ip(CleanIP),
    IP = CleanIP.

% Validate IP address format
validate_ip(IP) :-
    split_string(IP, ".", "", Parts),
    length(Parts, 4),
    maplist(number_string, [A,B,C,D], Parts),
    maplist(between(0, 255), [A,B,C,D]).