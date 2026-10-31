#!/usr/bin/env python3
"""Test client of the unrealircd skill, runs inside the container, see ircd.sh.

  ircclient.py <nick> <channel> [<blowkey>]

Connects to the server on 127.0.0.1:6667 and joins the channel. With a blowkey (8 to 56 bytes), it
encrypts what it says with FiSH ECB and decrypts what it receives, independently of slFtp's code.

/tmp/<nick>.in   FIFO for commands, one per line: "say <text>" or "raw <irc line>"
/tmp/<nick>.log  the received channel text, one "<sender> <text>" per line, and "JOINED" once the client
                 is in the channel; text that isn't valid FiSH shows as "<sender> [undecrypted] <text>"
"""
import os
import select
import socket
import sys
import warnings

from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes

FISH_B64 = "./0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


def fish_cipher(key):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")  # Blowfish is deprecated, but FiSH needs it
        return Cipher(algorithms.Blowfish(key.encode()), modes.ECB())


def fish_encrypt(key, text):
    data = text.encode(errors="surrogateescape")
    data += b"\0" * (-len(data) % 8)
    enc = fish_cipher(key).encryptor()
    data = enc.update(data) + enc.finalize()
    out = []
    for i in range(0, len(data), 8):
        left = int.from_bytes(data[i:i + 4], "big")
        right = int.from_bytes(data[i + 4:i + 8], "big")
        for value in (right, left):
            for _ in range(6):
                out.append(FISH_B64[value & 0x3F])
                value >>= 6
    return "+OK " + "".join(out)


def fish_decrypt(key, text):
    """The plain text, or None if text isn't valid FiSH"""
    b64 = text[4:]
    if not text.startswith("+OK ") or not b64 or len(b64) % 12 or any(c not in FISH_B64 for c in b64):
        return None
    data = b""
    for i in range(0, len(b64), 12):
        right = sum(FISH_B64.index(b64[i + k]) << (6 * k) for k in range(6))
        left = sum(FISH_B64.index(b64[i + 6 + k]) << (6 * k) for k in range(6))
        if left >> 32 or right >> 32:  # 6 characters can hold 36 bits
            return None
        data += left.to_bytes(4, "big") + right.to_bytes(4, "big")
    dec = fish_cipher(key).decryptor()
    return (dec.update(data) + dec.finalize()).rstrip(b"\0").decode(errors="backslashreplace")


def main():
    nick, channel = sys.argv[1:3]
    key = sys.argv[3] if len(sys.argv) > 3 else ""
    fifo_path = f"/tmp/{nick}.in"
    if not os.path.exists(fifo_path):
        os.mkfifo(fifo_path)
    # read and write, so that the FIFO doesn't report end of file after each writer
    fifo = os.open(fifo_path, os.O_RDWR | os.O_NONBLOCK)
    log = open(f"/tmp/{nick}.log", "w", buffering=1, encoding="utf-8")
    sock = socket.create_connection(("127.0.0.1", 6667))
    sock.sendall(f"NICK {nick}\r\nUSER {nick} 0 * :{nick}\r\n".encode())
    from_server = b""
    from_fifo = b""

    while True:
        readable, _, _ = select.select([sock, fifo], [], [])
        if sock in readable:
            data = sock.recv(65536)
            if not data:
                log.write("DISCONNECTED\n")
                return
            from_server += data
            *lines, from_server = from_server.split(b"\r\n")
            for line in lines:
                handle_line(sock, log, line.decode(errors="backslashreplace"), channel, key)
        if fifo in readable:
            from_fifo += os.read(fifo, 65536)
            *commands, from_fifo = from_fifo.split(b"\n")
            out = []
            for command in commands:
                # surrogateescape keeps bytes that aren't UTF-8 as they are, also when sending them
                verb, _, text = command.decode(errors="surrogateescape").rstrip("\r").partition(" ")
                if verb == "say":
                    out.append(f"PRIVMSG {channel} :{fish_encrypt(key, text) if key else text}\r\n")
                elif verb == "raw":
                    out.append(text + "\r\n")
            sock.sendall("".join(out).encode(errors="surrogateescape"))


def handle_line(sock, log, line, channel, key):
    parts = line.split(" ", 3)
    if parts[0] == "PING":
        sock.sendall(("PONG " + line.split(" ", 1)[1] + "\r\n").encode())
    elif len(parts) > 1 and parts[1] == "001":
        sock.sendall(f"JOIN {channel}\r\n".encode())
    elif len(parts) > 1 and parts[1] == "366":
        log.write("JOINED\n")
    elif len(parts) > 3 and parts[1] in ("PRIVMSG", "NOTICE"):
        sender = parts[0][1:].split("!")[0]
        text = parts[3][1:]
        if key:
            plain = fish_decrypt(key, text)
            text = plain if plain is not None else "[undecrypted] " + text
        log.write(f"<{sender}> {text}\n")
    elif len(parts) > 2 and parts[1] in ("JOIN", "PART", "QUIT"):
        log.write(f"{parts[1]} {parts[0][1:].split('!')[0]}\n")


if __name__ == "__main__":
    main()
