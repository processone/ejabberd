#!/bin/bash


set -v
sudo ejabberdctl stop
set -e
make
sudo make install
sudo ejabberdctl start
sleep 2
sudo ejabberdctl get_certificate all
sudo ejabberdctl list_certificates plain

sudo ejabberdctl revoke_certificate domain:my-test-ejabberd-server6.free 
sudo ejabberdctl list_certificates verbose
