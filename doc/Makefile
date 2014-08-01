# $Id$

SHELL = /bin/sh

CONTRIBUTED_MODULES = ""
#ifeq ($(shell ls mod_http_bind.tex),mod_http_bind.tex)
#	CONTRIBUTED_MODULES += "\\n\\setboolean{modhttpbind}{true}"
#endif


all:	release pdf html

release:
	@printf '%s\n' "Notes for the releaser:"
	@printf '%s\n' "* Do not forget to add a link to the release notes in guide.tex"
	@printf '%s\n' "* Do not forget to update the version number in ebin/ejabberd.app!"
	@printf '%s\n' "* Do not forget to update the features in introduction.tex (including \new{} and \improved{} tags)."
	@printf '%s\n' "Press any key to continue"
	##@read foo
	@printf '%s\n' "% ejabberd version (automatically generated)." > version.tex
	@printf '%s\n' "\newcommand{\version}{"`sed '/vsn/!d;s/\(.*\)"\(.*\)"\(.*\)/\2/' ../ebin/ejabberd.app`"}" >> version.tex
	@printf '%s'   "% Contributed modules (automatically generated)." > contributed_modules.tex
	@printf '%b\n' "$(CONTRIBUTED_MODULES)" >> contributed_modules.tex

html:	guide.html dev.html features.html

pdf:	guide.pdf features.pdf

clean:
	rm -f *.aux
	rm -f *.haux
	rm -f *.htoc
	rm -f *.idx
	rm -f *.ilg
	rm -f *.ind
	rm -f *.log
	rm -f *.out
	rm -f *.pdf
	rm -f *.toc
	[ ! -f contributed_modules.tex ] || rm contributed_modules.tex

distclean: clean
	rm -f *.html

guide.html:	guide.tex
		hevea -fix -pedantic guide.tex

dev.html:	dev.tex
		hevea -fix -pedantic dev.tex

features.html:	features.tex
		hevea -fix -pedantic features.tex

guide.pdf:	guide.tex
		pdflatex guide.tex
		pdflatex guide.tex
		pdflatex guide.tex
		makeindex guide.idx
		pdflatex guide.tex

features.pdf:	features.tex
		pdflatex features.tex
