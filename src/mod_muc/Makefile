# $Id$

include ../Makefile.inc

INCLUDES = -I/usr/lib/erlang/usr/include \
	   -I$(EI_DIR)/include \
	   -I/usr/local/include

LIBDIRS = -L$(EI_DIR)/lib -L/usr/local/lib


OUTDIR = ..
EFLAGS = -I .. -pz ..
OBJS   = \
	$(OUTDIR)/mod_muc.beam \
	$(OUTDIR)/mod_muc_room.beam

all:    $(OBJS)

$(OUTDIR)/%.beam:       %.erl
	erlc -W $(EFLAGS) -o $(OUTDIR) $<


clean:
	rm -f *.beam

TAGS:
	etags *.erl

