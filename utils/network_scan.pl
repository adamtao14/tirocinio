:- module(network_scanner, [scan_network/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).

% scan_network(+Subnet, -ActiveIPs)
% Example usage: scan_network('192.168.1.0/24', IPs).
scan_network(Subnet, ActiveIPs) :-
    format(string(Cmd), "nmap -sP ~w", [Subnet]),
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    string_codes(OutputStr, Codes),
    split_string(OutputStr, "\n", "", Lines),
    extract_active_ips(Lines, ActiveIPs).

% extract_active_ips(+Lines, -IPs)
extract_active_ips([], []).
extract_active_ips([Line|Rest], [IP|IPs]) :-
    sub_string(Line, _, _, _, "Nmap scan report for "),
    split_string(Line, " ", "", ["Nmap", "scan", "report", "for", IP]),
    !,
    extract_active_ips(Rest, IPs).
extract_active_ips([_|Rest], IPs) :-
    extract_active_ips(Rest, IPs).
