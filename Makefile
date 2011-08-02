.PHONY: rel deps test generate_snmp_header

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

test:   
	(cd test; make)

eunit:
	./rebar skip_deps=true eunit

rel: deps
	./rebar compile generate

generate_snmp_header: apps/ejabberd/include/EJABBERD-MIB.hrl

apps/ejabberd/include/EJABBERD-MIB.hrl: apps/ejabberd/priv/mibs/EJABBERD-MIB.bin
	erlc -o apps/ejabberd/include $<

apps/ejabberd/priv/mibs/EJABBERD-MIB.bin: apps/ejabberd/mibs/EJABBERD-MIB.mib
	erlc -o apps/ejabberd/priv/mibs/ $<

relclean:
	rm -rf rel/ejabberd
