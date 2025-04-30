% ==========================
% File: test/run_all_tests.pl
% Esegue tutti i test dei moduli in utils/
% ==========================

:- initialization(run_all_tests).

run_all_tests :-
	consult(test/test_command_runner),
	consult(test/test_file_utils),
	consult(test/test_time_utils),
	consult(test/test_graph_builder),
	consult(test/test_nmap_scanner),
	consult(test/test_shell),
	consult(test/test_proxmox),
	consult(test/test_http_server),
	%	consult(test/test_smb_ghost),
	run_tests.

% Utilizzo:
% ?- [test/run_all_tests].
