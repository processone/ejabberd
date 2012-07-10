#!/usr/bin/python

import sys
from struct import *

users = {}

def from_ejabberd():
    input_length = sys.stdin.read(2)
    (size,) = unpack('>h', input_length)
    return sys.stdin.read(size).split(':')

def to_ejabberd(bool):
    answer = 0
    if bool:
        answer = 1
    token = pack('>hh', 2, answer)
    sys.stdout.write(token)
    sys.stdout.flush()

def auth(username, server, password):
    p = ""
    if username in users:
        p = users[username]
    return p == password

def isuser(username, server):
    return username in users

def setpass(username, server, password):
    users[username] = password
    return True

def tryregister(username, server, password):
    if username in users:
        return False
    users[username] = password
    return True

def removeuser(username, server):
    del users[username]
    return True

while True:
    data = from_ejabberd()
    success = False
    if data[0] == "auth":
        success = auth(data[1], data[2], data[3])
    elif data[0] == "isuser":
        success = isuser(data[1], data[2])
    elif data[0] == "setpass":
        success = setpass(data[1], data[2], data[3])
    elif data[0] == "tryregister":
        success = tryregister(data[1], data[2], data[3])
    elif data[0] == "removeuser":
        success = removeuser(data[1], data[2])
    elif data[0] == "removeuser3":
        success = removeuser(data[1], data[2], data[3])
    to_ejabberd(success)

