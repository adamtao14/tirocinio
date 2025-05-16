:- module(port_scan, [scan_ports/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).

scan_ports(IP, OpenPorts) :-
    format(string(Cmd), "nmap -sS -Pn -p- --open -oG - ~w", [IP]),
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    string_codes(OutputStr, Codes),
    extract_open_ports(OutputStr, OpenPorts).

% Parse grepable nmap output for open TCP ports
extract_open_ports(Output, Ports) :-
    split_string(Output, "\n", "", Lines),
    include(is_ports_line, Lines, PortLines),
    maplist(extract_ports_from_line, PortLines, PortLists),
    flatten(PortLists, Ports).

is_ports_line(Line) :-
    sub_string(Line, _, _, _, "Ports: ").

extract_ports_from_line(Line, Ports) :-
    sub_string(Line, Start, _, _, "Ports: "),
    PortsStart is Start + 7,
    sub_string(Line, PortsStart, _, 0, PortPart),
    split_string(PortPart, ",", " ", RawPorts),
    include(valid_open_tcp_port, RawPorts, CleanPorts),
    maplist(extract_port_number, CleanPorts, Ports).

valid_open_tcp_port(Entry) :-
    sub_string(Entry, _, _, _, "/open/tcp").

extract_port_number(Entry, PortNum) :-
    split_string(Entry, "/", "", [PortStr|_]),
    number_string(PortNum, PortStr).
