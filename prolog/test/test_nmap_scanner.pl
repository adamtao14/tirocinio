% ================================
% File: test/test_nmap_scanner.pl
% Unit test per il modulo nmap_scanner.pl
% ================================

:- begin_tests(nmap_scanner).

:- use_module(scanner/nmap_scanner).
:- use_module(library(xpath)).
:- use_module(library(sgml)).

%% Test: estrazione degli host da un output XML di nmap
%  Verifica che `extract_hosts_from_xml/2` sia in grado di
%  estrarre una lista di tuple del tipo ("HOSTNAME", "IPv4")
%  a partire da un output XML di nmap.
test(extract_hosts_from_xml) :-
	DesiredHosts = [("", "10.1.146.11"), ("", "10.1.146.110"), ("", "10.1.146.111")],
	read_file_to_string("test/full-scan.xml", XMLText, []),
	extract_hosts_from_xml(XMLText, Hosts),
	assertion(Hosts == DesiredHosts).

%% Test: estrazione dei servizi TCP da un output XML di nmap
%  Verifica che `extract_hosts_from_xml/2` sia in grado di
%  estrarre una lista di tuple del tipo
%  ("IPv4",  "PROTOCOL", PORT, "NAME", "VERSION")
%  a partire da un output XML di nmap.
test(extract_tcp_services_from_xml) :-
	DesiredTCPServices = [
		("10.1.146.11", "tcp", 135, "msrpc", ""), 
		("10.1.146.11", "tcp", 139, "netbios-ssn", ""), 
		("10.1.146.11", "tcp", 445, "microsoft-ds", ""), 
		("10.1.146.11", "tcp", 3389, "ms-wbt-server", ""), 
		("10.1.146.11", "tcp", 5432, "postgresql", "9.6.2"), 
		("10.1.146.110", "tcp", 135, "msrpc", ""), 
		("10.1.146.110", "tcp", 139, "netbios-ssn", ""), 
		("10.1.146.110", "tcp", 445, "microsoft-ds", ""), 
		("10.1.146.110", "tcp", 3389, "ms-wbt-server", ""), 
		("10.1.146.111", "tcp", 135, "msrpc", ""), 
		("10.1.146.111", "tcp", 139, "netbios-ssn", ""), 
		("10.1.146.111", "tcp", 445, "microsoft-ds", "")
	],
	read_file_to_string("test/full-scan.xml", XMLText, []),
	extract_tcp_services_from_xml(XMLText, TCPServices),
	assertion(TCPServices = DesiredTCPServices).

%% Test: estrazione dei kernel guess da un output XML di nmap
%  Verifica che `extract_kernel_guesses_from_xml/2` sia in
%  grado di estrarre una lista di tuple del tipo
%  ("IPv4", "NAME", "VERSION", ACCURACY)
%  a partire da un output XML di nmap.
test(extract_kernel_guesses_from_xml) :-
	DesiredKernelGuesses = [
		("10.1.146.11", "Microsoft Windows Server 2016", "Windows", "2016", 97),
		("10.1.146.11", "Microsoft Windows Server 2016 build 10586 - 14393", "Windows", "2016", 97),
		("10.1.146.11", "Microsoft Windows 10 10586 - 14393", "Windows", "10", 95),
		("10.1.146.11", "Microsoft Windows 10 1507 - 1607", "Windows", "10", 95),
		("10.1.146.11", "Microsoft Windows Server 2012 R2 Update 1", "Windows", "2012", 95),
		("10.1.146.11", "Windows Server 2012 R2", "Windows", "2012", 95),
		("10.1.146.11", "Microsoft Windows 7, Windows Server 2012, or Windows 8.1 Update 1", "Windows", "7", 95),
		("10.1.146.11", "Microsoft Windows 7, Windows Server 2012, or Windows 8.1 Update 1", "Windows", "2012", 95),
		("10.1.146.11", "Microsoft Windows 7, Windows Server 2012, or Windows 8.1 Update 1", "Windows", "8.1", 95),
		("10.1.146.11", "Microsoft Server 2008 R2 SP1", "Windows", "2008", 94),
		("10.1.146.11", "Microsoft Windows Server 2012 or Server 2012 R2", "Windows", "2012", 94),
		("10.1.146.11", "Microsoft Windows 10", "Windows", "10", 92),
		("10.1.146.110", "Microsoft Windows 10 1507 - 1607", "Windows", "10", 100),
		("10.1.146.111", "Microsoft Windows 10 1709 - 1909", "Windows", "10", 100)
	],
	read_file_to_string("test/full-scan.xml", XMLText, []),
	extract_kernel_guesses_from_xml(XMLText, KernelGuesses),
	assertion(KernelGuesses = DesiredKernelGuesses).

%% Test: estrazione di informazioni da un output XML di nmap
%  Verifica che `parse_nmap_xml/i4` sia in
%  grado di estrarre informazioni dall'output XML di una
%  scansione nmap aggressiva.
test(parse_nmap_xml) :-
	DesiredHosts = [("", "10.1.146.11"), ("", "10.1.146.110"), ("", "10.1.146.111")],
	DesiredTCPServices = [
		("10.1.146.11", "tcp", 135, "msrpc", ""), 
		("10.1.146.11", "tcp", 139, "netbios-ssn", ""), 
		("10.1.146.11", "tcp", 445, "microsoft-ds", ""), 
		("10.1.146.11", "tcp", 3389, "ms-wbt-server", ""), 
		("10.1.146.11", "tcp", 5432, "postgresql", "9.6.2"), 
		("10.1.146.110", "tcp", 135, "msrpc", ""), 
		("10.1.146.110", "tcp", 139, "netbios-ssn", ""), 
		("10.1.146.110", "tcp", 445, "microsoft-ds", ""), 
		("10.1.146.110", "tcp", 3389, "ms-wbt-server", ""), 
		("10.1.146.111", "tcp", 135, "msrpc", ""), 
		("10.1.146.111", "tcp", 139, "netbios-ssn", ""), 
		("10.1.146.111", "tcp", 445, "microsoft-ds", "")
	],
	DesiredKernelGuesses = [
		("10.1.146.11", "Microsoft Windows Server 2016", "Windows", "2016", 97),
		("10.1.146.11", "Microsoft Windows Server 2016 build 10586 - 14393", "Windows", "2016", 97),
		("10.1.146.11", "Microsoft Windows 10 10586 - 14393", "Windows", "10", 95),
		("10.1.146.11", "Microsoft Windows 10 1507 - 1607", "Windows", "10", 95),
		("10.1.146.11", "Microsoft Windows Server 2012 R2 Update 1", "Windows", "2012", 95),
		("10.1.146.11", "Windows Server 2012 R2", "Windows", "2012", 95),
		("10.1.146.11", "Microsoft Windows 7, Windows Server 2012, or Windows 8.1 Update 1", "Windows", "7", 95),
		("10.1.146.11", "Microsoft Windows 7, Windows Server 2012, or Windows 8.1 Update 1", "Windows", "2012", 95),
		("10.1.146.11", "Microsoft Windows 7, Windows Server 2012, or Windows 8.1 Update 1", "Windows", "8.1", 95),
		("10.1.146.11", "Microsoft Server 2008 R2 SP1", "Windows", "2008", 94),
		("10.1.146.11", "Microsoft Windows Server 2012 or Server 2012 R2", "Windows", "2012", 94),
		("10.1.146.11", "Microsoft Windows 10", "Windows", "10", 92),
		("10.1.146.110", "Microsoft Windows 10 1507 - 1607", "Windows", "10", 100),
		("10.1.146.111", "Microsoft Windows 10 1709 - 1909", "Windows", "10", 100)
	],
	read_file_to_string("test/full-scan.xml", XMLText, []),
	parse_nmap_xml(XMLText, Hosts, TCPServices, KernelGuesses),
	assertion(Hosts == DesiredHosts),
	assertion(TCPServices = DesiredTCPServices),
	assertion(KernelGuesses = DesiredKernelGuesses).

%%% RIVEDERE DA QUI IN POI %%%

%% Test: scansione di un obiettivo
%  Verifica che `scan_target/4` sia in grado di effettuare
%  una scansione aggressiva con nmap sull'obiettivo
%  (specificato nel formato CIDR), producendo un output in
%  formato XML con il tag <nmaprun> ed impostando il
%  predicato dinamico scanned_target(CIDR, XML).
%  Il predicato ritorna true se riesce a completare la
%  scansione correttamente e a generare il predicato,
%  false altrimenti.
test(scan_target) :-
	CIDR = "10.1.146.0/24",
	Username = "kali",
	Password = "kali",
	retractall(scanned_target(CIDR, _)),
	once(scan_target(CIDR, XML, Username, Password)),
	scanned_target(CIDR, XML, Username, Password),
	assertion(string(XML)),
	assertion(XML \= ""),
	assertion(sub_string(XML, _, _, _, "<nmaprun")),
	retractall(scanned_target(CIDR, _, Username, Password)).

:- end_tests(nmap_scanner).
