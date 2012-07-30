.PHONY: rel deps test show_test_results generate_snmp_header 

EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
EJD_PRIV_MIB = $(EJD_PRIV)/mibs
EJD_MIB = $(EJABBERD_DIR)/mibs
DEVNODES = node1 node2 
TESTNODES = internal_mnesia internal_redis odbc_mnesia odbc_redis external_mnesia external_redis

all: deps compile

compile: rebar generate_snmp_header
	./rebar compile

deps: rebar generate_snmp_header
	./rebar get-deps

clean: rebar
	./rebar clean

test: test_deps
	cd test/ejabberd_tests; make

show_test_results:
	$$BROWSER `ls -td test/ct_report/ct_run.test@*/index.html | head -n 1` & disown

eunit: rebar
	./rebar skip_deps=true eunit

rel: rebar deps
	./rebar compile generate -f

devrel: $(DEVNODES)

$(DEVNODES): rebar deps compile
	@echo "building $@"
	mkdir -p dev
	(cd rel && ../rebar generate -f target_dir=../dev/ejabberd_$@ overlay_vars=./reltool_vars/$@_vars.config)
	cp apps/ejabberd/src/*.erl dev/ejabberd_$@/lib/ejabberd-2.1.8/ebin/
ifeq ($(shell uname), Linux)
	cp -R `dirname $(shell readlink -f $(shell which erl))`/../lib/tools-* dev/ejabberd_$@/lib/
else
	cp -R `which erl`/../../lib/tools-* dev/ejabberd_$@/lib/
endif

devclean:
	rm -rf dev/*

generate_snmp_header: apps/ejabberd/include/EJABBERD-MIB.hrl

$(EJD_INCLUDE)/EJABBERD-MIB.hrl: $(EJD_PRIV_MIB)/EJABBERD-MIB.bin
	erlc -o $(EJD_INCLUDE) $<

$(EJD_PRIV_MIB)/EJABBERD-MIB.bin: $(EJD_MIB)/EJABBERD-MIB.mib $(EJD_MIB)/EJABBERD-MIB.funcs
	erlc -o $(EJD_PRIV_MIB) $<

relclean:
	rm -rf rel/ejabberd

COMBO_PLT = $(HOME)/.esl_ejabberd_combo_dialyzer_plt
PLT_LIBS  = $(wildcard rel/ejabberd/lib/*/ebin)

DIALYZER_APPS = ejabberd
DIALYZER_APPS_PATHS = $(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

check_plt: rel
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

build_plt: rel
	dialyzer --build_plt --output_plt $(COMBO_PLT) $(PLT_LIBS)

dialyzer: compile
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	rm $(COMBO_PLT)

test_deps: rebar
	./rebar -C rebar.tests.config get-deps

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

