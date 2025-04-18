
% Facts: open_port(PortNumber).
open_port(22).
open_port(80).
open_port(443).

% Sample ports to scan
port_to_scan(21).
port_to_scan(22).
port_to_scan(80).
port_to_scan(8080).
port_to_scan(443).

% Rule: scan_port(Port, Result)
scan_port(Port, open) :- open_port(Port).
scan_port(Port, closed) :- \+ open_port(Port).

% Rule: scan_all_ports(ResultList)
scan_all_ports(ResultList) :-
    findall(Port-Status, (port_to_scan(Port), scan_port(Port, Status)), ResultList).

% Top-level predicate to check if any ports are open
open_ports_found(Ports) :-
    findall(Port, (port_to_scan(Port), open_port(Port)), Ports),
    Ports \= [].

% Main check: port_scan_possible is true if any open port is found
port_scan_possible :-
    open_ports_found(_).
