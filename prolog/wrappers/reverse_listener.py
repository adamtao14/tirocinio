#!/usr/bin/env python3

import argparse
import os
import threading
import signal
import sys
from pwn import listen,context

#def receive_loop(conn, outfile, interactive):
#    while True:
#        try:
#            data = conn.recv(timeout=5)
#            if not data:
#                continue
#            decoded = data.decode(errors='ignore')
#            if interactive:
#                print(decoded, end="", flush=True)
#            if not interactive and outfile:
#                outfile.write(decoded)
#                outfile.flush()
#        except EOFError:
#            break
#        except Exception:
#            continue
def receive_loop(conn, outfile, interactive):
    while True:
        try:
            data = conn.recv(timeout=5)
            if not data:
                continue
            decoded = data.decode(errors='ignore')
            if interactive:
                print(decoded, end="", flush=True)
            elif outfile:
                outfile.write(decoded)
                outfile.flush()
        except EOFError:
            #break
            print("EOF raggiunto")
            continue
        except Exception as e:
            #print("Eccezione receive ricevuta: ", str(e))
            continue

def send_loop(conn, infile, interactive):
    while True:
        try:
            if interactive:
                print(end=" ", flush=True)  # No prompt, stay on same line
                cmd = input()
            else:
                cmd = infile.readline()
                if not cmd:
                    continue
            if cmd.strip().lower() == "exit":
                break
            conn.send((cmd.strip() + "\n").encode())
        except Exception as e:
            #print("Eccezione send ricevuta: ", str(e))
            continue
#except (EOFError, KeyboardInterrupt):
#    break

def pipe_mode(conn, input_path, output_path):
    with open(input_path, 'r') as infile, open(output_path, 'w') as outfile:
        receiver = threading.Thread(target=receive_loop, args=(conn, outfile, False), daemon=True)
        sender = threading.Thread(target=send_loop, args=(conn, infile, False), daemon=True)
        receiver.start()
        sender.start()
        receiver.join()
        sender.join()

def interactive_mode(conn):
    receiver = threading.Thread(target=receive_loop, args=(conn, None, True), daemon=True)
    sender = threading.Thread(target=send_loop, args=(conn, None, True), daemon=True)
    receiver.start()
    sender.start()
    receiver.join()
    sender.join()

def cleanup_pipes(input_path, output_path):
    try:
        if os.path.exists(input_path):
            os.remove(input_path)
        if os.path.exists(output_path):
            os.remove(output_path)
    except Exception as e:
        print(f"[!] Error cleaning up pipes: {e}")

def main():
    parser = argparse.ArgumentParser(description="Reverse shell listener with input/output redirection.")
    parser.add_argument('--in', dest='input_path', default='/tmp/in', help='Named pipe for input')
    parser.add_argument('--out', dest='output_path', default='/tmp/out', help='Named pipe for output')
    parser.add_argument('--port', dest='port', type=int, required=True, help='Port to listen on')
    parser.add_argument('--interactive', action='store_true', help='Enable interactive terminal mode')
    parser.add_argument('--cleanup-on-exit', action='store_true', help='Delete pipes on exit')

    args = parser.parse_args()

    # Gestione cleanup da segnali
    def handle_signal(signum, frame):
        if args.cleanup_on_exit and not args.interactive:
            cleanup_pipes(args.input_path, args.output_path)
        sys.exit(0)

    if not args.interactive:
        context.log_level = 'error'
        for path in [args.input_path, args.output_path]:
            if not os.path.exists(path):
                os.mkfifo(path)
                os.chmod(path, 0o666)

    if args.cleanup_on_exit:
        signal.signal(signal.SIGINT, handle_signal)
        signal.signal(signal.SIGTERM, handle_signal)

    if args.interactive:
        print(f"[+] Waiting for connection on port {args.port}...")
    conn = listen(args.port)
    conn.wait_for_connection()
    if args.interactive:
        print("[+] Reverse shell established!")

    try:
        if args.interactive:
            interactive_mode(conn)
        else:
            pipe_mode(conn, args.input_path, args.output_path)
    finally:
        conn.close()
        if args.interactive:
            print("[*] Connection closed.")
        if args.cleanup_on_exit and not args.interactive:
            cleanup_pipes(args.input_path, args.output_path)

if __name__ == "__main__":
    main()
