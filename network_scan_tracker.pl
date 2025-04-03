:- module(network_scan_tracker, [
    scannable_from/2,
    rescan_from/2,
    host_alive/1,
    host_from/3,
    scan_done/2,
    list_alive_hosts/0,
    list_host_sources/1,
    extract_tcp_services_from_xml/2
]).

:- dynamic kernel_guess/4.
:- dynamic tcp_service/6.
:- dynamic host_alive/1.
:- dynamic scan_done/2.
:- dynamic host_from/3.

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(command_utils).

% Scans a target from a source IP if not already done, using XML output with service detection
scannable_from(Source, Target) :-
    scan_done(Source, Target), !.
scannable_from(Source, Target) :-
    run_elevated_command(
        nmap,
        ['-A', '-T5', '-S', Source, Target, '-oX', '-'],
        Result,
        stdout_stderr(Out, _)
    ),
    Result == true,
    extract_alive_hosts_from_xml(Out, AliveHosts),
    maplist(assertz_host(Source, Target), AliveHosts),
    extract_tcp_services_from_xml(Out, TCPServices),
    maplist(assertz_tcp_service, TCPServices),
    assertz(scan_done(Source, Target)).

% Force re-scan by removing previous results from this scan
rescan_from(Source, Target) :-
    retractall(scan_done(Source, Target)),
    findall(IP, host_from(Source, Target, IP), IPs),
    retractall(host_from(Source, Target, _)),
    maplist(retract_host_if_no_other_origin(Source, Target), IPs),
    scannable_from(Source, Target).

retract_host_if_no_other_origin(Source, Target, IP) :-
    \+ (host_from(OtherSource, OtherTarget, IP),
        (OtherSource \= Source ; OtherTarget \= Target)),
    retractall(host_alive(IP)).

assertz_host(Source, Target, IP) :-
    assertz(host_alive(IP)),
    assertz(host_from(Source, Target, IP)).

assertz_tcp_service((IP, PortId, Protocol, ServiceName, Product, Version)) :-
    assertz(tcp_service(IP, PortId, Protocol, ServiceName, Product, Version)).

assertz_kernel_guess(IP, KernelName, KernelVersion, Accuracy) :-
    assertz(kernel_guess(IP, KernelName, KernelVersion, Accuracy)).

list_alive_hosts :-
    forall(host_alive(IP), writeln(IP)).

list_host_sources(IP) :-
    forall(host_from(Source, Target, IP),
           format("~w trovato da ~w --> ~w~n", [IP, Source, Target])).

% Extract IPs from Nmap XML output (with optional DTD validation)
extract_alive_hosts_from_xml(XMLText, Hosts) :-
    setup_call_cleanup(
        open_string(XMLText, Stream),
        load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
        close(Stream)
    ),
    findall(IP,
        ( xpath(XML, //host, Host),
          xpath(Host, status(@state), up),
          xpath(Host, address(@addr), IP)
        ),
        Hosts
    ).

% extract_tcp_services(+XMLText)
extract_tcp_services(XMLText) :-
    setup_call_cleanup(
        open_string(XMLText, Stream),
        load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
        close(Stream)
    ),
    findall(Host, xpath(XML, //(host), Host), Hosts),
    forall(
        member(Host, Hosts),
        (
            xpath(Host, address(@addrtype=ipv4), element(address, AddressAttrs, _)),
            member(addr=IP, AddressAttrs),
            findall(Port, xpath(Host, ports/port, Port), Ports),
            forall(
                member(Port, Ports),
                (
                    xpath(Port, /self(@protocol), Protocol),
                    xpath(Port, /self(@portid), PortId),
                    xpath(Port, service(@name), ServiceName),
                    ( xpath(Port, service(@product), Product) -> true ; Product = '' ),
                    ( xpath(Port, service(@version), Version) -> true ; Version = '' ),
                    assertz(tcp_service(IP, PortId, Protocol, ServiceName, Product, Version))
                )
            )
        )
    ).

% extract_tcp_services_from_xml(+XMLText)
extract_tcp_services_from_xml(XMLText, TCPServices) :-
    setup_call_cleanup(
        open_string(XMLText, Stream),
        load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
        close(Stream)
    ),
    findall(Host, xpath(XML, //(host), Host), Hosts),
    findall(
        (IP, PortId, Protocol, ServiceName, Product, Version),
        (
            member(Host, Hosts),
            xpath(Host, address(@addrtype=ipv4), element(address, AddressAttrs, _)),
            member(addr=IP, AddressAttrs),
            xpath(Host, ports/port, Port),
            xpath(Port, /self(@protocol), Protocol),
            xpath(Port, /self(@portid), PortId),
            xpath(Port, service(@name), ServiceName),
            ( xpath(Port, service(@product), Product) -> true ; Product = '' ),
            ( xpath(Port, service(@version), Version) -> true ; Version = '' )
        ),
        TCPServices
    ).

extract_kernel_guesses_from_xml(XMLText, KernelGuesses) :-
    setup_call_cleanup(
        open_string(XMLText, Stream),
        load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
        close(Stream)
    ),
    findall(Host, xpath(XML, //(host), Host), Hosts),
    findall((IP, KernelName, KernelVersion, Accuracy), (
        member(Host, Hosts),
        xpath(Host, address(@addrtype=ipv4), element(address, AddressAttrs, _)),
        member(addr=IP, AddressAttrs),
        xpath(Host, os/osmatch, element(osmatch, MatchAttrs, _)),
        member(accuracy=AccuracyStr, MatchAttrs),
        member(name=NameStr, MatchAttrs),
        sub_atom(NameStr, _, _, _, 'Linux'),  % solo Linux per ora
        split_string(NameStr, " ", "", Tokens),
        Tokens = [KernelNameStr, KernelVersionStr | _],  % es. "Linux 3.10"
        atom_string(KernelName, KernelNameStr),
        atom_string(KernelVersion, KernelVersionStr),
        atom_number(AccuracyStr, Accuracy)
    ), KernelGuesses).


father(john,mike).

son(mike, john).

