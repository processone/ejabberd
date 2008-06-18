APPNAME = ejabberd
VSN = $(shell sed '/vsn/!d;s/\(.*\)"\(.*\)"\(.*\)/\2/' ../../src/ejabberd.app)

DOCDIR=.
SRCDIR=../../src

.PHONY = all

all: docs

clean:
	rm -f *.html
	rm edoc-info
	rm erlang.png

docs:
	erl -noshell -run edoc_run application \
	"'$(APPNAME)'" '"$(SRCDIR)"' '[{dir,"$(DOCDIR)"},{packages, false},{todo,true},{private,true},{def,{vsn,"$(VSN)"}},{stylesheet,"process-one.css"},{overview,"$(DOCDIR)/overview.edoc"}]' -s init stop
