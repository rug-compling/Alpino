#!/usr/bin/env python3

import socket

def alpino_parse(sent, host='localhost', port=42424):
    s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    s.connect((host,port))
    sent = sent + "\n\n"
    s.sendall(sent.encode('utf-8'))
    total_xml=[]
    while True:
        xml = s.recv(8192)
        if not xml:
            break
        total_xml.append(xml.decode('utf8'))

    return "".join(total_xml)


def main():
    sentence=input("Geef de zin die Alpino moet analyseren: ")
    xml=alpino_parse(sentence)
    print(xml)


if __name__ == '__main__':
    main()
