.PHONY: rel deps test show_test_results generate_snmp_header

EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
EJD_PRIV_MIB = $(EJD_PRIV)/mibs
EJD_MIB = $(EJABBERD_DIR)/mibs


all: deps compile

compile: generate_snmp_header
	./rebar compile

deps: generate_snmp_header
	./rebar get-deps

clean:
	./rebar clean

test:   
	(cd test; make)

show_test_results:
	$$BROWSER `ls -td test/ct_report/ct_run.test@*/index.html | head -n 1` & disown

eunit:
	./rebar skip_deps=true eunit

rel: deps
	./rebar compile generate

generate_snmp_header: apps/ejabberd/include/EJABBERD-MIB.hrl

$(EJD_INCLUDE)/EJABBERD-MIB.hrl: $(EJD_PRIV_MIB)/EJABBERD-MIB.bin
	erlc -o $(EJD_INCLUDE) $<

$(EJD_PRIV_MIB)/EJABBERD-MIB.bin: $(EJD_MIB)/EJABBERD-MIB.mib $(EJD_MIB)/EJABBERD-MIB.funcs
	erlc -o $(EJD_PRIV_MIB) $<

relclean:
	rm -rf rel/ejabberd
