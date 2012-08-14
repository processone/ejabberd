.PHONY: rel deps test show_test_results generate_snmp_header 

EJABBERD_DIR = apps/ejabberd
EJD_INCLUDE = $(EJABBERD_DIR)/include
EJD_PRIV = $(EJABBERD_DIR)/priv
EJD_PRIV_MIB = $(EJD_PRIV)/mibs
EJD_MIB = $(EJABBERD_DIR)/mibs


all: deps compile

compile: rebar generate_snmp_header
	./rebar compile

deps: rebar generate_snmp_header
	./rebar get-deps

clean: rebar
	./rebar clean

test: test/Makefile
	cd test; make

test/Makefile:
	git submodule update --init --recursive

show_test_results:
	$$BROWSER `ls -td test/ct_report/ct_run.test@*/index.html | head -n 1` & disown

eunit: rebar
	./rebar skip_deps=true eunit

rel: rebar deps
	./rebar compile generate -f

devrel: rebar deps compile
	mkdir -p dev
	(cd rel && ../rebar generate -f target_dir=../dev/ejabberd overlay_vars=./reltool_vars/node1_vars.config)
	cp apps/ejabberd/src/*.erl dev/ejabberd/lib/ejabberd-2.1.8/ebin/
	cp -R `which erl`/../../lib/tools-* dev/ejabberd/lib/
	(cd rel && ../rebar generate -f target_dir=../dev/ejabberd2 overlay_vars=./reltool_vars/node2_vars.config)

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

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

