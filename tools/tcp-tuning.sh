#!/bin/sh 
ulimit -n 562524 
# /etc/security/limits.conf
#@ejabberd       soft    nofile           562000
#@ejabberd       hard    nofile           562000
# 4KB send buffer, 20,480 connections max at worst case 
#echo 83886080  > /proc/sys/net/core/wmem_max 
#echo 83886080  > /proc/sys/net/core/wmem_default 
echo 327680000  > /proc/sys/net/core/wmem_max 
echo 327680000  > /proc/sys/net/core/wmem_default 
# 16KB receive buffer, 20,480 connections max at worst case 
#echo 335544320 > /proc/sys/net/core/rmem_max 
#echo 335544320 > /proc/sys/net/core/rmem_default 
echo 1310720000 > /proc/sys/net/core/rmem_max
echo 1310720000 > /proc/sys/net/core/rmem_default
# Max open files 
echo 562524 > /proc/sys/fs/file-max 
# Fast port recycling (TIME_WAIT) 
echo 1 >/proc/sys/net/ipv4/tcp_tw_recycle 
echo 1 >/proc/sys/net/ipv4/tcp_tw_reuse 
# TIME_WAIT buckets increased 
echo 360000 > /proc/sys/net/ipv4/tcp_max_tw_buckets 
# FIN timeout decreased 
echo 15 > /proc/sys/net/ipv4/tcp_fin_timeout 
# SYN backlog increased 
echo 65536 > /proc/sys/net/ipv4/tcp_max_syn_backlog 
# SYN cookies enabled 
echo 1 > /proc/sys/net/ipv4/tcp_syncookies 
# Local port range maximized 
echo "1024 65535" > /proc/sys/net/ipv4/ip_local_port_range 
# Netdev backlog increased 
echo 100000 > /proc/sys/net/core/netdev_max_backlog 
# Do no save route metrics
echo 1 > /proc/sys/net/ipv4/route/flush
echo 30 > /proc/sys/net/ipv4/tcp_fin_timeout
# Interface transmit queuelen increased 
ifconfig eth0 txqueuelen 10000 

