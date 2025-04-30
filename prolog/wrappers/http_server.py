#!/usr/bin/env python3

import http.server
import socketserver
import argparse
import os


def main():
    parser = argparse.ArgumentParser(description="Simple HTTP Server for File Hosting")
    parser.add_argument('--port', type=int, default=8000, help='Port to listen on (default: 8000)')
    parser.add_argument('--directory', type=str, default='.', help='Directory to serve (default: current directory)')
    args = parser.parse_args()

    os.chdir(args.directory)
    handler = http.server.SimpleHTTPRequestHandler
    print("PORT: ", args.port)
    with socketserver.TCPServer(("0.0.0.0", args.port), handler) as httpd:
        print(f"[+] Serving '{args.directory}' on port {args.port} (http://0.0.0.0:{args.port}/)")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n[!] Shutting down server.")


if __name__ == '__main__':
    main()
