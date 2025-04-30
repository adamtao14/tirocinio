:- begin_tests(graph_builder).
:- use_module(utils/graph_builder).
:- use_module(utils/command_runner).
:- use_module(utils/file_utils).

%% test: aggiunta di un nodo al grafo
%  verifica che `add_node/1` sia in grado di aggiungere
%  un nodo nuovo di nome Name al grafo.
test(add_new_node) :-
	NodeName = "a",
	assertion(add_node(NodeName)),
	assertion(node(NodeName)),
	assertion(add_node(NodeName)).

%% test: aggiunta di un nodo al grafo
%  verifica che `add_node/1` ritorni true
%  e non crei predicati node() doppioni se si
%  prova ad inserire un nodo esistente nel
%  grafo.
test(add_existing_node) :-
	NodeName = "a",
	findall(Name, node(Name), Nodes),
	length(Nodes, CountBefore),
	assertion(add_node(NodeName)),
	assertion(node(NodeName)),
	findall(Name, node(Name), Nodes),
	length(Nodes, CountAfter),
	assertion(CountBefore = CountAfter).

%% test: aggiunta di un arco al grafo
%  verifica che `add_edge/3` sia in grado di aggiungere
%  due nuovi nodi e un nuovo arco tra gli stessi,
%  ritornando infine true.
test(add_new_edge_on_new_nodes) :-
	StartNode = "b",
	EndNode = "c",
	Label = "another b-c edge",
	assertion(add_edge(StartNode, EndNode, Label)),
	assertion(node(StartNode)),
	assertion(node(EndNode)),
	assertion(edge(StartNode, EndNode, Label)).

%% test: aggiunta di un arco al grafo
%  verifica che `add_edge/3` sia in grado di aggiungere
%  un nuovo arco tra due nodi esistenti, non aumentando
%  il numero di predicati node() esistenti e ritornando
%  true.
test(add_new_edge_on_existing_nodes) :-
	StartNode = "b",
	EndNode = "c",
	Label = "b-c edge",
	findall(Name, node(StartNode), BNodesBefore),
	length(BNodesBefore, BCountBefore),
	findall(Name, node(EndNode), CNodesBefore),
	length(CNodesBefore, CCountBefore),
	assertion(add_edge(StartNode, EndNode, Label)),
	findall(Name, node(StartNode), BNodesAfter),
	length(BNodesAfter, BCountAfter),
	findall(Name, node(EndNode), CNodesAfter),
	length(CNodesAfter, CCountAfter),
	assertion(BCountBefore = BCountAfter),
	assertion(CCountBefore = CCountAfter),
	assertion(node(StartNode)),
	assertion(node(EndNode)),
	assertion(edge(StartNode, EndNode, Label)).

%% test: rimozione di un arco dal grafo
%  verifica che `del_edge/3` sia in grado di rimuovere
%  un arco tra due nodi esistenti, ritornando infine
%  true.
test(del_existing_edge_from_existing_nodes) :-
	StartNode = "b",
	EndNode = "c",
	Label = "another b-c edge",
	assertion(del_edge(StartNode, EndNode, Label)),
	assertion(\+ edge(StartNode, EndNode, Label)),
	assertion(node(StartNode)),
	assertion(node(EndNode)).

%% test: rimozione di un arco dal grafo
%  verifica che `del_edge/3` ritorni true se si prova
%  a rimuovere un arco inesistente tra due nodi
%  esistenti, preservando i nodi.
test(del_nonexisting_edge_from_existing_nodes) :-
	StartNode = "b",
	EndNode = "c",
	Label = "nonexisting",
	assertion(del_edge(StartNode, EndNode, Label)),
	assertion(\+ edge(StartNode, EndNode, Label)),
	assertion(node(StartNode)),
	assertion(node(EndNode)).

%% test: rimozione di un arco dal grafo
%  verifica che `del_edge/3` ritorni true se si prova
%  a rimuovere un arco inesistente tra due nodi
%  inesistenti.
test(del_nonexisting_edge_from_nonexisting_nodes) :-
	StartNode = "x",
	EndNode = "y",
	Label = "nonexisting",
	assertion(del_edge(StartNode, EndNode, Label)),
	assertion(\+ edge(StartNode, EndNode, Label)),
	assertion(\+ node(StartNode)),
	assertion(\+ node(EndNode)).

%% test: rimozione di un nodo dal grafo
%  verifica che `del_node/1` sia in grado
%  di rimuovere un nodo esistente e gli
%  archi incidenti su esso, ritornando
%  infine true.
test(del_existing_node) :-
	NodeName = "b",
	assertion(del_node(NodeName)),
	assertion(\+ node(NodeName)),
	assertion(\+ edge(NodeName, _, _)),
	assertion(\+ edge(_, NodeName, _)).

%% test: rimozione di un nodo dal grafo
%  verifica che `del_node/1` ritorni true se
%  si tenta a rimuovere un nodo (e gli archi
%  incidenti) inesistente.
test(del_nonexisting_node) :-
	NodeName = "nonexisting",
	assertion(del_node(NodeName)),
	assertion(\+ node(NodeName)),
	assertion(\+ edge(NodeName, _, _)),
	assertion(\+ edge(_, NodeName, _)).

%% test: cancellazione di un grafo
%  verifica che `clear_graph/0` sia in grado di
%  cancellare tutti i predicati dinamici node/1
%  e edge/3, per poi ritornare true.
test(clear_graph) :-
	assertion(clear_graph),
	assertion(\+ node(_)),
	assertion(\+ edge(_, _, _)).

%% test: generazione del grafo
%  verifica che `generate_dot_lines/0` sia in grado di
%  generare il corpo di un file in formato DOT contenente
%  le definizioni dei nodi e degli archi, per poi ritornare
%  true.
test(generate_dot_lines_from_nonempty_graph) :-
	DesiredLines = [
		'digraph AttackGraph {',
		'  "Start";',
		'  "Enumerate";',
		'  "Exploit";',
		'  "PrivEsc";',
		'  "Goal";',
		'  "End";',
		'  "Start" -> "Enumerate" [label="nmap -A"];',
		'  "Enumerate" -> "Exploit" [label="SMBGhost.py"];',
		'  "Exploit" -> "PrivEsc" [label="ConPTY"];',
		'  "PrivEsc" -> "Goal" [label="read flag.txt"];',
		'  "Goal" -> "End" [label="Done!"];',
		'}'
	],
	add_node("Start"),
	add_node("Enumerate"),
	add_node("Exploit"),
	add_node("PrivEsc"),
	add_node("Goal"),
	add_node("End"),
	add_edge("Start", "Enumerate", "nmap -A"),
	add_edge("Enumerate", "Exploit", "SMBGhost.py"),
	add_edge("Exploit", "PrivEsc", "ConPTY"),
	add_edge("PrivEsc", "Goal", "read flag.txt"),
	add_edge("Goal", "End", "Done!"),
	findall(N, node(N), Nodes),
	findall(edge(From, To, Label), edge(From, To, Label), Edges),
	graph_builder:generate_dot_lines(Nodes, Edges, Lines),
	assertion(Lines = DesiredLines),
	clear_graph.

%% test: generazione del grafo
%  verifica che `generate_dot_lines/0` sia in grado di
%  generare il solo header del file dot se il grafo è
%  stato cancellato con clear_graph.
test(generate_dot_lines_from_empty_graph, [setup(clear_graph), cleanup(clear_graph)]) :-
	DesiredLines = [
		'digraph AttackGraph {',
		'}'
	],
	clear_graph,
	findall(N, node(N), Nodes),
	findall(edge(F, T, L), edge(F, T, L), Edges),
	graph_builder:generate_dot_lines(Nodes, Edges, Lines),
	assertion(Lines == DesiredLines).

%% test: produzione del grafo in formato DOT
%  verifica che `create_dot_file/3` sia in grado di
%  generare un file DOT completo a partire dalla
%  rappresentazione del grafo e salvarlo sul file
%  system.
test(create_dot_file) :-
	DOTFile = "test/attack-graph.dot",
	DesiredLines = [
		"digraph G {",
		"  \"Start\";",
		"  \"Enumerate\";",
		"  \"Exploit\";",
		"  \"PrivEsc\";",
		"  \"Goal\";",
		"  \"End\";",
		"  \"Start\" -> \"Enumerate\" [label=\"nmap -A\"];",
		"  \"Enumerate\" -> \"Exploit\" [label=\"SMBGhost.py\"];",
		"  \"Exploit\" -> \"PrivEsc\" [label=\"ConPTY\"];",
		"  \"PrivEsc\" -> \"Goal\" [label=\"read flag.txt\"];",
		"  \"Goal\" -> \"End\" [label=\"Done!\"];",
		"}"
	],
	string_lines(DesiredString, DesiredLines),
	add_node("Start"),
	add_node("Enumerate"),
	add_node("Exploit"),
	add_node("PrivEsc"),
	add_node("Goal"),
	add_node("End"),
	add_edge("Start", "Enumerate", "nmap -A"),
	add_edge("Enumerate", "Exploit", "SMBGhost.py"),
	add_edge("Exploit", "PrivEsc", "ConPTY"),
	add_edge("PrivEsc", "Goal", "read flag.txt"),
	add_edge("Goal", "End", "Done!"),
	findall(N, node(N), Nodes),
	findall(edge(From, To, Label), edge(From, To, Label), Edges),
	assertion(create_dot_file(Nodes, Edges, DOTFile)),
	assertion(exists_file(DOTFile)),
	read_file_to_string(DOTFile, Content, []),
	assertion(Content = DesiredString).

%% test: disegno del grafo
%  verifica che `export_dot_to_png/1` sia in grado di
%  generare un file immagine in formato PNG a partire
%  dal file in formato DOT.
test(export_dot_to_png) :-
	DOTFile = "test/attack-graph.dot",
	file_name_extension(Base, _, DOTFile),
	file_name_extension(Base, "png", PNGFile),
	assertion(export_dot_to_png(DOTFile)),
	exists_file(PNGFile),
	once(run_command("file", [PNGFile], false, false, Output, _, _)),
	assertion(once(sub_string(Output, _, _, _, "PNG image"))).

:- end_tests(graph_builder).
