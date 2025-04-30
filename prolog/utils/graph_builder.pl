/** <graph_builder> Utilità per il disegno di grafi
 *
 *  Questo modulo introduce diversi predicati di utilità 
 *  per la creazione e il disegno di grafi tramite il
 *  software Graphviz (che deve essere già installato).
 *
 *  @author Mauro Andreolini
 *  @version 0.1
 */

:- module(graph_builder, [
	add_node/1,
	node/1,
	add_edge/3,
	edge/3,
	del_edge/3,
	del_node/1,
	clear_graph/0,
	create_dot_file/3,
	export_dot_to_png/1
]).

:- use_module(utils/command_runner).

:- dynamic node/1.
:- dynamic edge/3.

%% add_node(+Name:string) is det
%
%  Il predicato add_node/1 aggiunge un nodo al grafo
%  se non è già presente, e ritorna true.
%  Se il nodo è già presente, ritorna semplicemente true.
%
%  @param Name Il nome del nodo del grafo da aggiungere.
%
add_node(Name) :-
	( node(Name) -> true ; assertz(node(Name)) ).

%% add_edge(+From:string, +To:string, +Label:string) is det
%
%  Il predicato add_edge/3 aggiunge due nodi al grafo
%  e un arco fra i due nodi, dopodiché ritorna true.
%  Se i nodi e l'arco sono già esistenti, ritorna true.
%
%  @param From Il nome del nodo di partenza dell'arco.
%  @param To Il nome del nodo di arrivo dell'arco.
%  @param Label L'etichetta dell'arco.
%
add_edge(From, To, Label) :-
	add_node(From),
	add_node(To),
	assertz(edge(From, To, Label)).

%% del_edge(+From:string, +To:string, +Label:string) is det
%
%  Il predicato add_edge/3 rimuove un arco due nodi,
%  dopodiché ritorna true.
%  Se i nodi e l'arco non esistono, ritorna true.
%
%  @param From Il nome del nodo di partenza dell'arco.
%  @param To Il nome del nodo di arrivo dell'arco.
%  @param Label L'etichetta dell'arco.
%
del_edge(From, To, Label) :-
	retractall(edge(From, To, Label)).

%% del_node(+Name:string) is det
%
%  Il predicato del_node/1 rimuove un nodo e tutti i suoi
%  archi incidenti dal grafo se è già presente, e ritorna
%  true. Se il nodo non è presente, ritorna semplicemente
%  true.
%
%  @param Name Il nome del nodo del grafo da rimuovere.
%
del_node(Name) :-
	retractall(edge(Name, _, _)),
	retractall(edge(_, Name, _)),
	retractall(node(Name)).

%% clear_graph() is det
%
%  Il predicato clear_graph/0 cancella il grafo rimuovendo
%  tutti i predicati dinamici node/1 e edge/3, ritornando
%  infine true.
%
clear_graph :-
	retractall(node(_)),
	retractall(edge(_, _, _)).

%% generate_dot_lines(+Nodes:list, +Edges:list, -Lines:list) is det
%
%  Il predicato generate_dot_lines/3 riceve in ingresso
%  la lista dei nomi dei nodi, la lista delle relazioni
%  edge/3 e crea una lista di righe in formato DOT,
%  rappresentanti la definizione dei nodi e degli archi
%  del grafo. Infine ritorna true.
%
%  @param Nodes La lista contenente i nomi dei nodi.
%  @param Edges La lista contenente i predicati edge/3.
%  @param Lines La lista contenente le righe del corpo
%               del file in formato DOT.
%
generate_dot_lines(Nodes, Edges, [
	'digraph AttackGraph {' |
	BodyLines
]) :-
	findall(Line, (
		member(N, Nodes),
		format(atom(Line), '  "~w";', [N])
	), NodeLines),
	findall(Line, (
		member(edge(F, T, L), Edges),
		format(atom(Line), '  "~w" -> "~w" [label="~w"];', [F, T, L])
	), EdgeLines),
	append(NodeLines, EdgeLines, All),
	append(All, ['}'], BodyLines).

%% create_dot_file(+Nodes:list, +Edges:list, +DOTFile:string) is det
%
%  Il predicato create_dot_graph/3 crea un file in
%  formato DOT a partire dalla lista dei nomi di
%  nodi Nodes e dalla lista delle relazioni edge/3
%  Edges, dopodiché lo salva su disco con il nome
%  Outfile. Infine, ritorna true.
%
%  @param Nodes La lista contenente i nomi dei nodi.
%  @param Edges La lista contenente i predicati edge/3.
%  @param DOTFile Il nome del file DOT da salvare.
%
create_dot_file(Nodes, Edges, DOTFile) :-
	open(DOTFile, write, Stream),
	write(Stream, 'digraph G {\n'),
	forall(member(Node, Nodes), format(Stream, '  "~w";\n', [Node])),
	forall(member(edge(From, To, Label), Edges),
		   format(Stream, '  "~w" -> "~w" [label="~w"];\n', [From, To, Label])),
	write(Stream, '}\n'),
	close(Stream).

%% export_dot_to_png(+DOTFile:string) is det
%
%  Il predicato export_dot_to_png/1 crea una immagine in
%  formato PNG a partire da un file in formato DOT che
%  esprime il grafo di attacco.
%
%  @param DOTFile Il percorso del file contenente il DOT.
%
export_dot_to_png(DOTFile) :-
	file_name_extension(Base, _, DOTFile),
	file_name_extension(Base, "png", PNGFile),
	run_command('dot', ['-Tpng', DOTFile, '-o', PNGFile], false, false, _, _, _).
