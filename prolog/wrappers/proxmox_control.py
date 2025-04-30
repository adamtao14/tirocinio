#!/usr/bin/env python3

import argparse
from proxmoxer import ProxmoxAPI

def get_proxmox(host, user, password, verify_ssl):
	return ProxmoxAPI(host, user=user, password=password, verify_ssl=verify_ssl)

def start_vm(proxmox, node, vmid):
	return proxmox.nodes(node).qemu(vmid).status.start.post()

def stop_vm(proxmox, node, vmid):
	return proxmox.nodes(node).qemu(vmid).status.stop.post()

def get_vm_status(proxmox, node, vmid):
    return proxmox.nodes(node).qemu(vmid).status.current.get()

def rollback_snapshot(proxmox, node, vmid, snapshot):
	return proxmox.nodes(node).qemu(vmid).snapshot(snapshot).rollback.post()

def main():
	parser = argparse.ArgumentParser(description="Interazione base con Proxmox VE via API.")
	parser.add_argument('--host', required=True, help='Indirizzo del nodo Proxmox (es. 10.0.0.1)')
	parser.add_argument('--user', required=True, help='Utente Proxmox (es. root@pam)')
	parser.add_argument('--password', required=True, help='Password utente')
	parser.add_argument('--node', required=True, help='Nome del nodo (es. pve)')
	parser.add_argument('--vmid', type=int, help='ID della VM')
	parser.add_argument('--snapshot', help='Nome dello snapshot')
	parser.add_argument('--start', action='store_true', help='Avvia la VM')
	parser.add_argument('--stop', action='store_true', help='Ferma la VM')
	parser.add_argument('--status', action='store_true', help='Mostra lo stato corrente della VM')
	parser.add_argument('--rollback', action='store_true', help='Rollback allo snapshot')
	parser.add_argument('--no-verify', action='store_true', help='Disattiva verifica del certificato SSL')

	args = parser.parse_args()

	proxmox = get_proxmox(args.host, args.user, args.password, not args.no_verify)

	if args.start and args.vmid:
		print(f"[+] Avvio della VM {args.vmid}")
		print(start_vm(proxmox, args.node, args.vmid))

	elif args.stop and args.vmid:
		print(f"[+] Arresto della VM {args.vmid}")
		print(stop_vm(proxmox, args.node, args.vmid))

	elif args.status and args.vmid:
		status_info = get_vm_status(proxmox, args.node, args.vmid)
		print(status_info.get('status', 'unknown'))

	elif args.rollback and args.vmid and args.snapshot:
		print(f"[+] Rollback VM {args.vmid} allo snapshot '{args.snapshot}'")
		print(rollback_snapshot(proxmox, args.node, args.vmid, args.snapshot))

	else:
		print("[-] Nessuna azione specificata. Usa --start, --stop o --rollback.")

if __name__ == "__main__":
	main()
