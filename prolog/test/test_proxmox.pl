:- begin_tests(proxmox).
:- use_module(infra/proxmox).

%% Test: impostazione delle credenziali utente per Proxmox 
%  Verifica che `set_proxmox_credentials/2` sia in grado
%  di creare un predicato dinamico
%  proxmox_credentials(Username, Password) contenente le
%  credenziali dell'utente abilitato a gestire l'istanza
%  di Proxmox VE.
%  Alla fine del test si cancella il predicato dinamico
%  creato per non sporcare l'ambiente.
test(set_proxmox_credentials) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	once(set_proxmox_credentials(Username, Password)),
	once(proxmox:proxmox_credentials(DesiredUsername, DesiredPassword)),
	assertion(Username == DesiredUsername),
	assertion(Password == DesiredPassword),
	retractall(proxmox_credentials(_, _)).

%% Test: lettura dello stato di una VM
%  Verifica che `status_vm/4` sia in grado di ritornare
%  una stringa "running" o "stopped" su una VM con un
%  VMID valido.
test(status_vm_valid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	VMID = 205,
	set_proxmox_credentials(Username, Password),
	once(status_vm(Host, Node, VMID, Output)),
	assertion(Output == "running"; Output == "stopped"),
	retractall(proxmox_credentials(_, _)).

%% Test: lettura dello stato di una VM
%  Verifica che `status_vm/4` ritorni false su una VM
%  con un VMID invalido.
test(status_vm_invalid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	VMID = 111,
	set_proxmox_credentials(Username, Password),
	assertion(\+ status_vm(Host, Node, VMID, _)),
	retractall(proxmox_credentials(_, _)).

%% Test: avvio di una VM
%  Verifica che `start_vm/3` sia in grado di avviare
%  una VM con un VMID valido e che lo stato sia
%  "running".
test(start_vm_valid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	VMID = 205,
	set_proxmox_credentials(Username, Password),
	once(start_vm(Host, Node, VMID)),
	sleep(1),
	once(status_vm(Host, Node, VMID, Output)),
	assertion(Output == "running"),
	retractall(proxmox_credentials(_, _)).

%% Test: avvio di una VM
%  Verifica che `start_vm/3` ritorni false su una VM
%  con un VMID invalido.
test(start_vm_invalid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	VMID = 111,
	set_proxmox_credentials(Username, Password),
	assertion(\+ start_vm(Host, Node, VMID)),
	retractall(proxmox_credentials(_, _)).

%% Test: terminazione di una VM
%  Verifica che `stop_vm/3` sia in grado di terminare 
%  una VM con un VMID valido e che lo stato sia "stopped".
test(stop_vm_valid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	VMID = 205,
	set_proxmox_credentials(Username, Password),
	once(stop_vm(Host, Node, VMID)),
	sleep(1),
	once(status_vm(Host, Node, VMID, Output)),
	assertion(Output == "stopped"),
	retractall(proxmox_credentials(_, _)).

%% Test: terminazione di una VM
%  Verifica che `stop_vm/3` ritorni false su una VM
%  con un VMID invalido.
test(stop_vm_invalid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	VMID = 111,
	set_proxmox_credentials(Username, Password),
	assertion(\+ stop_vm(Host, Node, VMID)),
	retractall(proxmox_credentials(_, _)).

%% Test: rollback di una VM ad uno snapshot
%  Verifica che `rollback_vm/4` sia in grado di reimpostare
%  una VM con un VMID valido ad uno snapshot specifico e
%  che lo stato sia "running". NON si verifica che lo
%  snapshot sia quello esatto.
test(rollback_vm_valid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	Snapshot = "ready_for_attack",
	VMID = 205,
	set_proxmox_credentials(Username, Password),
	once(rollback_vm(Host, Node, VMID, Snapshot)),
	sleep(1),
	once(status_vm(Host, Node, VMID, Output)),
	assertion(Output == "running"),
	retractall(proxmox_credentials(_, _)).

%% Test: rollback di una VM ad uno snapshot
%  Verifica che `rollback_vm/4` ritorni false su una VM
%  con un VMID invalido.
test(stop_vm_invalid_VMID) :-
	Username = "andreolini@pve",
	Password = "aExyy1oJqP",
	Host = "10.90.21.200",
	Node = "cecasp",
	Snapshot = "ready_for_attack",
	VMID = 111,
	set_proxmox_credentials(Username, Password),
	assertion(\+ rollback_vm(Host, Node, VMID, Snapshot)),
	retractall(proxmox_credentials(_, _)).

:- end_tests(proxmox).
