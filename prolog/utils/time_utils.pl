/** <time_utils> Utilità per l'esecuzione dei timestamp UNIX
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per la gestione dei timestamp e per il calcolo delle
 *  durate degli intervalli temporali.
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(time_utils, [
	timestamp/1,
	timestamp_diff/3
]).

:- use_module(library(system)).

%% timestamp(-Time:number) is det
%
%  Il predicato timestamp/2 ritorna il timestamp UNIX
%  (numero di secondi a partire dal 1/1/1970) relativo
%  all'istante corrente.
%
%  @param Time Il timestamp ritornatoda get_time().
%
timestamp(Time) :-
	get_time(Time).

%% timestamp_diff(+T1:number, +T2:number, -Delta:number) is det
%
%  Il predicato timestamp_diff/3 ritorna la differenza in
%  secondi tra due timestamp UNIX.
%
%  @param T1 Il timestamp di inizio intervallo.
%  @param T2 Il timestamp di fine intervallo.
%  @param Delta La differenza tra i due intervalli.
%
timestamp_diff(T1, T2, Delta) :-
	number(T1),
	number(T2),
	Delta is T2 - T1.
