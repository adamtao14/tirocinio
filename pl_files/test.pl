host_alive('192.168.1.1').
host_alive('192.168.1.2').
tcp_service('192.168.1.1', 22, tcp, ssh, 'OpenSSH', '8.2').
scan_done('192.168.1.100', '192.168.1.1').
