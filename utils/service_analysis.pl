:- module(service_analysis, [analyze_services/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(lists)).

% Entry point
analyze_services(IP, Services) :-
    format(string(Cmd), "nmap -sV ~w -oN -", [IP]),
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    string_codes(OutputStr, Codes),
    extract_services(OutputStr, Services).

% Extract relevant services from Nmap normal output
extract_services(OutputStr, Services) :-
    split_string(OutputStr, "\n", "", Lines),
    include(is_port_line, Lines, PortLines),
    maplist(parse_port_line, PortLines, Services).

% Identify lines that start with a port format, e.g., "22/tcp"
is_port_line(Line) :-
    split_string(Line, " ", "", [First | _]),
    sub_string(First, _, _, _, "/tcp"). % crude match, improve if needed

% Parse a line like:
% 22/tcp   open  ssh      OpenSSH 3.9p1 (protocol 1.99)
parse_port_line(Line, service(Port, Proto, Name, Version)) :-
    split_string(Line, " ", " \t", Tokens0),
    exclude(==( ""), Tokens0, Tokens),  % remove empty strings
    Tokens = [PortProto, "open", Name | VersionTokens],
    split_string(PortProto, "/", "", [PortStr, Proto]),
    number_string(Port, PortStr),
    atomic_list_concat(VersionTokens, " ", VersionAtom),
    atom_string(VersionAtom, Version).
