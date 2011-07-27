#!/bin/sh

erlc -I ../apps/ejabberd/include/ fill_roster.erl
erlc -I ../apps/ejabberd/include/ privacy.erl
