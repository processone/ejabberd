#!/usr/bin/env python

'''
Created on 2011-08-12

@author: radoslaw.szymczyszyn@erlang-solutions.com
'''

import os, re, sys, traceback

from zabbix_api import ZabbixAPI, ZabbixAPIException

# options

server       = "http://127.0.0.1/zabbix"
username     = "admin"
password     = "zabbix"
hostname     = "Zabbix Server"
ejabberd_hrl = os.path.join(os.path.dirname(__file__),
                            "../apps/ejabberd/include/EJABBERD-MIB.hrl")

# don't touch below (unless u know what u r doin)

def all_counters(header_file):
    cmd = ("grep -- '-define' %s |" \
        + "grep -Ev 'default_|low_|high_|_instance|monitorGroup|-define\((route|ejabberd)|true\)'") \
        % header_file
    def line2counter(l):
        key, l = l[8:].split(',', 1)
        oid = l.split('[')[1].split(']')[0].replace(',', '.')
        return ('ejabberd.' + key, oid + '.0')
    with os.popen(cmd) as f:
        counters = map(
            line2counter,
            f.readlines())
    return counters

def add_counters(hrl):
    for key, oid in all_counters(hrl):
        try:
            add_counter(key, oid)
        except ZabbixAPIException:
            traceback.print_tb(sys.exc_traceback)

def add_counter(key, oid, description=None):
    #print oid, description, key
    if key == 'ejabberd.generalNodeName':
        value_type = '1'
    else:
        value_type = '3'
    zapi.item.create({ 'delay' : '5',
                       'hostid' : hostid,
                       'description' :
                            description if description != None else key,
                       'key_' : key,
                       'type' : '4',  # SNMPv2
                       'value_type' : value_type,
                       'snmp_port' : '50910', 
                       'snmp_oid' : oid,
                       'snmp_community' : 'public' })

def get_item(key):
    spec = { 'output' : 'extend',
             'filter' : {'key_' : key} }
    r = zapi.item.get(spec)
    #print r
    return r

def print_item(item):
    for k,v in ( (k,item[k]) for k in sorted(item.keys())):
        print k,v

def main():
    global zapi, hostid

    zapi = ZabbixAPI(server=server, path="", log_level=6)
    zapi.login(username, password)

    hostid = zapi.host.get({"filter":{"host":hostname}})[0]["hostid"]
    #print hostid

    add_counters(hrl)

    # ...or list specified item
    #print_item( get_item('ejabberd.generalNodeName')[0] )

if __name__ == '__main__':
    main()
