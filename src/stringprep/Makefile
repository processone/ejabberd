# $Id$

include ../Makefile.inc

INCLUDES = -I$(ERLANG_DIR)/usr/include \
	   -I$(EI_DIR)/include \
	   -I/usr/local/include

LIBDIRS = -L$(EI_DIR)/lib -L/usr/local/lib

ERLSHLIBS = ../stringprep_drv.so



OUTDIR = ..
EFLAGS = -I .. -pz ..
OBJS   = \
	$(OUTDIR)/stringprep.beam

all:    $(OBJS) $(ERLSHLIBS)

$(OUTDIR)/%.beam:       %.erl
	erlc -W $(EFLAGS) -o $(OUTDIR) $<



#all:	$(ERLSHLIBS)
#	erl -s make all report "{outdir, \"..\"}" -noinput -s erlang halt

$(ERLSHLIBS):	../%.so:	%.c uni_data.c uni_norm.c
			gcc -Wall -O2 $(INCLUDES) $(LIBDIRS) \
			$(subst ../,,$(subst .so,.c,$@)) \
			-lerl_interface \
			-lei \
			-o $@ -fpic -shared \

clean:
	rm -f *.beam

TAGS:
	etags *.erl

