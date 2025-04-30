:- begin_tests(time_utils).
:- use_module(utils/time_utils).

%% Test: lettura di un timestamp UNIX
%  Verifica che `timestamp/1` ritorni numeri interi non
%  decrescenti. Ci si fida sul fatto che siano il numero
%  di secondi dall'epoca UNIX (1/1/1970).
test(timestamp) :-
	timestamp(T1),
	timestamp(T2),
	assertion(number(T1)),
	assertion(number(T2)),
	assertion(T1 =< T2).

%% Test: calcolo della durata di un intervallo
%  Verifica che `timestamp_diff/3` ritorni la
%  durata corretta di un intervallo temporale.
%  decrescenti. Ci si fida sul fatto che siano il numero
%  di secondi dall'epoca UNIX (1/1/1970).
%  Se almeno un parametro non è un numero, ritorna false.
test(timestamp_diff_invalid) :-
	TValid = 10,
	TInvalid = "timestamp",
	assertion(\+ timestamp_diff(TValid, TInvalid, _)),
	assertion(\+ timestamp_diff(TInvalid, TValid, _)),
	assertion(\+ timestamp_diff(TInvalid, TInvalid, _)).

%% Test: calcolo della durata di un intervallo
%  Verifica che `timestamp_diff/3` ritorni la
%  durata corretta di un intervallo temporale.
%  decrescenti. Ci si fida sul fatto che siano il numero
%  di secondi dall'epoca UNIX (1/1/1970).
%  Se i parametri sono corretti, ritorna la durata
%  dell'intervallo in secondi.
test(timestamp_diff_invalid) :-
	DesiredDelta = 1,
	timestamp(T1),
	sleep(DesiredDelta),
	timestamp(T2),
	timestamp_diff(T1, T2, Delta),
	floor(Delta, IntDelta),
	assertion(IntDelta = DesiredDelta).

:- end_tests(time_utils).

