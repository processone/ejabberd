APPNAME = ejabberd
VSN = SVN

DOCDIR=.
SRCDIR=../../src

.PHONY = all

all: docs

clean:
	rm -f *.html
	rm edoc-info

docs:
	erl -noshell -run edoc_run application \
	"'$(APPNAME)'" '"$(SRCDIR)"' '[{dir,"$(DOCDIR)"},{packages, false},{todo,true},{private,true},{def,{vsn,"$(VSN)"}},{stylesheet,"process-one.css"},{overview,"$(DOCDIR)/overview.edoc"}]' -s init stop
