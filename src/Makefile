# $Id$

INCLUDES = -I/usr/lib/erlang/usr/include \
	   -I/usr/lib/erlang/lib/erl_interface-3.3.0/include

LIBDIRS = -L/usr/lib/erlang/lib/erl_interface-3.3.0/lib

all:	expat_erl.so
	erl -make

expat_erl.so:	expat_erl.c
		gcc -Wall $(INCLUDES) $(LIBDIRS) \
		-lexpat \
		expat_erl.c \
		-lerl_interface \
		-lei \
		-o expat_erl.so -fpic -shared \

