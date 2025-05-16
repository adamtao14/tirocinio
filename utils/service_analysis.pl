:- module(service_analysis, [analyze_services/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).

% Entry point: analyze_services(+IP, -ServiceNames)
analyze_services(IP, ServiceNames) :-
    format(string(Cmd), "nmap -sV -Pn ~w -oG -", [IP]),
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out),
    string_codes(OutputStr, Codes),
    extract_service_names(OutputStr, ServiceNames).

% Extract list of service names
extract_service_names(Output, Names) :-
    split_string(Output, "\n", "", Lines),
    include(is_ports_line, Lines, PortLines),
    maplist(parse_service_line, PortLines, ServiceLists),
    flatten(ServiceLists, Services),
    findall(Name, member(service(_, _, Name), Services), Names).

is_ports_line(Line) :-
    sub_string(Line, _, _, _, "Ports: ").

parse_service_line(Line, Services) :-
    sub_string(Line, Start, _, _, "Ports: "),
    PortsStart is Start + 7,
    sub_string(Line, PortsStart, _, 0, PortPart),
    split_string(PortPart, ",", " ", RawPorts),
    include(valid_open_service_entry, RawPorts, CleanPorts),
    maplist(parse_service_entry, CleanPorts, Services).

valid_open_service_entry(Entry) :-
    sub_string(Entry, _, _, _, "/open/").

parse_service_entry(Entry, service(Port, Protocol, Name)) :-
    split_string(Entry, "/", "", [PortStr, _Status, Protocol | Rest]),
    (Rest = [Name | _] ; Name = "unknown"),
    number_string(Port, PortStr).
