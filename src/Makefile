# $Id$

INCLUDES = -I/usr/lib/erlang/usr/include \
	   -I/usr/lib/erlang/lib/erl_interface-3.3.0/include

LIBDIRS = -L/usr/lib/erlang/lib/erl_interface-3.3.0/lib

ERLSHLIBS = expat_erl.so

all:	$(ERLSHLIBS)
	erl -make

$(ERLSHLIBS):	%.so:	%.c
			gcc -Wall $(INCLUDES) $(LIBDIRS) \
			-lexpat \
			$(subst .so,.c,$@) \
			-lerl_interface \
			-lei \
			-o $@ -fpic -shared \

