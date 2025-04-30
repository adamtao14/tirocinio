/** <nmap_scanner> Utilità per la gestione di scansioni con NMap
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per l'esecuzione di scansioni con NMap e per l'estrazione
 *  di informazioni relative agli host in predicati.
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(nmap_scanner, [
	extract_hosts_from_xml/2,
	extract_tcp_services_from_xml/2,
	extract_kernel_guesses_from_xml/2,
	parse_nmap_xml/4,
	scanned_target/4,
	scan_target/4
]).

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(utils/command_runner).

:- dynamic scanned_target/4.

%% extract_hosts_from_xml(+XMLText:string, -Hosts:list) is det
%
%  Il predicato extract_hosts_from_xml/2 prende in ingresso
%  l'output XML di una scansione nmap aggressiva ed estrae
%  informazioni legate al nome degli host in una tupla del
%  tipo ("HOSTNAME", "IPv4").
%
%  @param XMLText La stringa contenente l'XML ottenuto dalla
%                 scansione.
%  @param Hosts La lista delle tuple ("HOSTNAME", "IPv4")
%               estratte dal file XML.
%
extract_hosts_from_xml(XMLText, Hosts) :-
	setup_call_cleanup(
		open_string(XMLText, Stream),
		load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
		close(Stream)
	),
	findall(
		(Hostname, IP),
		(
			xpath(XML, //host, Host),
			xpath(Host, status(@state), up),
			xpath(Host, address(@addrtype=ipv4), AddrNode),
			AddrNode = element(_, Attributes, _),
			memberchk(addr=AddrAtom, Attributes),
			atom_string(AddrAtom, IP),
			( xpath(Host, hostnames/hostname(@name), Hostname) -> true ; Hostname = "" )
		),
		Hosts
	).

%% extract_tcp_services_from_xml(+XMLText:string, -TCPServices:list) is det
%
%  Il predicato extract_tcp_services_from_xml/2 prende in
%  ingresso l'output XML di una scansione nmap aggressiva
%  ed estrae informazioni legate ai servizi TCP esposti
%  in una tupla di questo tipo:
%  ("IPv4", "PROTOCOL", PORT, "NAME", "VERSION")
%
%  @param XMLText La stringa contenente l'XML ottenuto dalla
%                 scansione.
%  @param TCPServices La lista delle tuple ("IPv4",
%                     "PROTOCOL", PORT, "NAME", "VERSION")
%                     estratte dal file XML.
%
extract_tcp_services_from_xml(XMLText, TCPServices) :-
	setup_call_cleanup(
		open_string(XMLText, Stream),
		load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
		close(Stream)
	),
	findall(
		(IP, Protocol, Port, Name, Version),
		(
			xpath(XML, //host, Host),
			xpath(Host, status(@state), up),
			xpath(Host, address(@addrtype=ipv4), AddrNode),
			AddrNode = element(_, AddrAttrs, _),
			memberchk(addr=AddrAtom, AddrAttrs),
			atom_string(AddrAtom, IP),

			xpath(Host, ports/port, PortNode),
			PortNode = element(port, PortAttrs, _),
			memberchk(protocol=ProtoAtom, PortAttrs),
			memberchk(portid=PortIDAtom, PortAttrs),
			atom_string(ProtoAtom, Protocol),
			atom_number(PortIDAtom, Port),

			xpath(PortNode, service(@name), NameAtom),
			atom_string(NameAtom, Name),

			( xpath(PortNode, service(@version), VersionAtom)
			  -> atom_string(VersionAtom, Version)
			  ;  Version = ""
			)
		),
        TCPServices
	).

%% extract_kernel_guesses_from_xml(+XMLText:string, -KernelGuesses:list) is det
%
%  Il predicato extract_kernel_guesses_from_xml/2 prende in
%  ingresso l'output XML di una scansione nmap aggressiva
%  ed estrae informazioni legate alle possibili versioni
%  del kernel in una tupla di questo tipo:
%  ("IPv4", "NAME", "VERSION", ACCURACY)
%
%  @param XMLText La stringa contenente l'XML ottenuto dalla
%                 scansione.
%  @param KernelGuesses La lista delle tuple
%                       "IPv4", "NAME", "VERSION", ACCURACY)
%                       estratte dal file XML.
%
extract_kernel_guesses_from_xml(XMLText, KernelGuesses) :-
	setup_call_cleanup(
		open_string(XMLText, Stream),
		load_structure(Stream, XML, [dialect(xml), space(remove), ignore_doctype(true)]),
		close(Stream)
	),
	findall(
		(IP, FullName, ProductName, ProductVersion, Accuracy),
		(
			xpath(XML, //host, Host),
			xpath(Host, status(@state), up),
			xpath(Host, address(@addrtype=ipv4), AddrNode),
			AddrNode = element(_, AddrAttrs, _),
			memberchk(addr=AddrAtom, AddrAttrs),
			atom_string(AddrAtom, IP),

			xpath(Host, os/osmatch, OSMatch),
			OSMatch = element(osmatch, MatchAttrs, _),
			memberchk(name=FullNameAtom, MatchAttrs),
			memberchk(accuracy=AccuracyAtom, MatchAttrs),

			atom_string(FullNameAtom, FullName),
			atom_number(AccuracyAtom, Accuracy),

			xpath(OSMatch, osclass, OSClass),
			OSClass = element(osclass, OSAttrs, _),
			memberchk(osfamily=ProductAtom, OSAttrs),
			memberchk(osgen=VersionAtom, OSAttrs),

			atom_string(ProductAtom, ProductName),
			atom_string(VersionAtom, ProductVersion)
		),
		KernelGuesses
	).

%% parse_nmap_xml(+XMLText:string, -Hosts:list, -TCPServices:list, -KernelGuesses:list) is det
%
%  Il predicato parse_nmap_xml/4 prende in ingresso
%  l'output XML di una scansione nmap aggressiva ed
%  estrae informazioni utili (hostname, indirizzi IPv4,
%  servizi TCP esposti, versioni del kernel) eseguendo
%  i seguenti predicati su di esso:
%  - extract_hosts_from_xml(XMLText, Hosts)
%  - extract_tcp_services_from_xml(XMLText, TCPServices)
%  - extract_kernel_guesses_from_xml(XMLText, KernelGuesses)
%  - tcp_service/5, tcp_service(IP, porta, proto, service, version)
%  - kernel_version/3 (IP, OS, version, accuracy)
%
%  @param Hosts La lista delle tuple ("HOSTNAME", "IPv4")
%               estratte dal file XML.
%  @param XMLText La stringa contenente l'XML ottenuto dalla
%                 scansione.
%  @param TCPServices La lista delle tuple ("IPv4",
%                     "PROTOCOL", PORT, "NAME", "VERSION")
%                     estratte dal file XML.
%  @param KernelGuesses La lista delle tuple
%                       "IPv4", "NAME", "VERSION", ACCURACY)
%                       estratte dal file XML.
%
parse_nmap_xml(XMLText, Hosts, TCPServices, KernelGuesses) :-
	extract_hosts_from_xml(XMLText, Hosts),
	extract_tcp_services_from_xml(XMLText, TCPServices),
	extract_kernel_guesses_from_xml(XMLText, KernelGuesses).

%% scan_target(+CIDR:string, Username:+string, Password:+string, -XML:string) is det
%
%  Il predicato scan_target/4 effettua una scansione
%  sull'obiettivo CIDR (host o rete in formato CIDR)
%  con il comando nmap e ritorna l'output in formato
%  XML.
%  Al termine della scansione viene generato un predicato
%  dinamico scanned_target(CIDR, XML). Ciò consente di
%  evitare la scansione di un obiettivo già scandito.
%  Per poter funzionare, il predicato scan_target/2
%  ha bisogno delle credenziali sudoer che eseguirà il
%  comando nmap con i privilegi di root tramite sudo.
%
%  @param CIDR L'obiettivo dell'analisi in formato CIDR.
%  @param Username Lo username abilitato all'esecuzione di
%                  comandi tramite sudo come utente root.
%  @param Password La password dell'utente abilitato alla
%                  esecuzione di sudo.
%  @param XMLText L'output XML prodotto dalla scansione.
%
scan_target(CIDR, XMLText, Username, Password) :-
	scanned_target(CIDR, XMLText, Username, Password), !.
scan_target(CIDR, XMLText, Username, Password) :-
	set_sudo_credentials(Username, Password),
	exec_as_root("nmap", ["--max-retries", "2", "--scan-delay", "1ms", "--exclude", "10.1.146.120", "--exclude", "10.1.146.1", "--exclude", "10.1.146.99", "-oX", "-", "-sS", CIDR], Username, Password, XMLText, _Stderr, ExitCode),
	ExitCode == 0,
	assertz(scanned_target(CIDR, XMLText, Username, Password)).
