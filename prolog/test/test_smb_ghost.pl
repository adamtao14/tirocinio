:- begin_tests(smb_ghost).
:- use_module(exploits/smb_ghost).

%% Test: controllo della vulnerabilità SMBGhost
%  Verifica che `check_vuln/3` sia in grado di
%  verificare la presenza della vulnerabilità
%  SMBGhost su un target specifico vulnerabile.
test(check_vuln_on_vulnerable_host) :-
	Target = "10.1.146.111",
	Port = 445,
	assertion(check_vuln(Target, Port)).

%% Test: controllo della vulnerabilità SMBGhost
%  Verifica che `check_vuln/3` sia in grado di
%  verificare l'assenza della vulnerabilità
%  SMBGhost su un target specifico non vulnerabile
%  (indirizzo IP sbagliato).
test(check_vuln_on_invalid_ip) :-
	Target = "10.1.146.113",
	Port = 445,
	assertion(\+ check_vuln(Target, Port)).

%% Test: controllo della vulnerabilità SMBGhost
%  Verifica che `check_vuln/3` sia in grado di
%  verificare l'assenza della vulnerabilità
%  SMBGhost su un target specifico non vulnerabile
%  (porta sbagliata).
test(check_vuln_on_invalid_ip) :-
	Target = "10.1.146.111",
	Port = 444,
	assertion(\+ check_vuln(Target, Port)).

:- end_tests(smb_ghost).
