.PHONY: all test

all: test

test_clean:
	rm -rf tests/*.beam
	make test

test: escalus
	rm -rf ct_report
	mkdir  ct_report
	erlc run_common_test.erl
	erl -noinput -sname test -pa `pwd`/tests -pa `pwd`/escalus/ebin `pwd`/escalus/deps/exmpp/ebin -s run_common_test ct

escalus:
	(cd escalus; make)

