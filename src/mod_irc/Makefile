# $Id$

include ../Makefile.inc

INCLUDES = -I$(ERLANG_DIR)/usr/include \
	   -I$(EI_DIR)/include \
	   -I/usr/local/include

LIBDIRS = -L$(EI_DIR)/lib -L/usr/local/lib

ERLSHLIBS = ../iconv_erl.so



OUTDIR = ..
EFLAGS = -I .. -pz ..
OBJS   = \
	$(OUTDIR)/mod_irc.beam \
	$(OUTDIR)/mod_irc_connection.beam \
	$(OUTDIR)/iconv.beam

all:    $(OBJS) $(ERLSHLIBS)

$(OUTDIR)/%.beam:       %.erl
	erlc -W $(EFLAGS) -o $(OUTDIR) $<



#all:	$(ERLSHLIBS)
#	erl -s make all report "{outdir, \"..\"}" -noinput -s erlang halt

$(ERLSHLIBS):	../%.so:	%.c
			gcc -Wall $(INCLUDES) $(LIBDIRS) \
			$(subst ../,,$(subst .so,.c,$@)) \
			-lerl_interface \
			-lei \
			-o $@ -fpic -shared \

clean:
	rm -f *.beam

TAGS:
	etags *.erl

