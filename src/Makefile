# $Id$

include Makefile.inc

INCLUDES = -I/usr/lib/erlang/usr/include \
	   -I$(EI_DIR)/include \
	   -I/usr/local/include

LIBDIRS = -L$(EI_DIR)/lib -L/usr/local/lib

ERLSHLIBS = expat_erl.so

all:	$(ERLSHLIBS)
	erl -s make all report -noinput -s erlang halt

$(ERLSHLIBS):	%.so:	%.c
			gcc -Wall $(INCLUDES) $(LIBDIRS) \
			-lexpat \
			$(subst .so,.c,$@) \
			-lerl_interface \
			-lei \
			-o $@ -fpic -shared \

clean:
	rm -f *.beam

Makefile.inc:
	./configure

TAGS:
	etags *.erl

