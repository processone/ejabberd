#!/bin/sh

# To format the source code, simply run:
#   make format

LOG=${1:-/tmp/ejabberd-format.log}
REBAR3=${2:-rebar3}

$REBAR3 efmt -w --parallel >$LOG 2>&1

if ! grep -q 'All files were formatted correctly' $LOG
then
    cat $LOG
    exit 1
fi
rm $LOG
