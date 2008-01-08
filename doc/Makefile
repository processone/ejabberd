# $Id$

CONTRIBUTED_MODULES = ""
ifeq ($(shell ls mod_http_bind.tex),mod_http_bind.tex)
	CONTRIBUTED_MODULES += "\\r\\n\\setboolean{modhttpbind}{true}"
endif
ifeq ($(shell ls mod_http_fileserver.tex),mod_http_fileserver.tex)
	CONTRIBUTED_MODULES += "\\r\\n\\setboolean{modhttpfileserver}{true}"
endif


all:	release pdf html

release:
	@echo "Notes for the releaser:"
	@echo "* Do not forget to add a link to the release notes in guide.tex"
	@echo "* Do not forget to update the version number in src/ejabberd.hrl!"
	@echo "* Do not forget to update the features in introduction.tex (including \new{} and \improved{} tags)."
	@echo "Press any key to continue"
	@read foo
	@echo "% ejabberd version (automatically generated)." > version.tex
	@echo "\newcommand{\version}{"`sed '/VERSION/!d;s/\(.*\)"\(.*\)"\(.*\)/\2/' ../src/ejabberd.hrl`"}" >> version.tex
	@echo -n "% Contributed modules (automatically generated)."  > contributed_modules.tex
	@echo "$(CONTRIBUTED_MODULES)" >> contributed_modules.tex

html:	guide.html dev.html features.html

pdf:	guide.pdf features.pdf

clean:
	rm -f *.aux
	rm -f *.haux
	rm -f *.html
	rm -f *.htoc
	rm -f *.idx
	rm -f *.ilg
	rm -f *.ind
	rm -f *.log
	rm -f *.out
	rm -f *.pdf
	rm -f *.toc

guide.html:	guide.tex
		hevea -fix -noiso -pedantic guide.tex

dev.html:	dev.tex
		hevea -fix -noiso -pedantic dev.tex

features.html:	features.tex
		hevea -fix -noiso -pedantic features.tex

guide.pdf:	guide.tex
		pdflatex guide.tex
		pdflatex guide.tex
		pdflatex guide.tex
		makeindex guide.idx
		pdflatex guide.tex

features.pdf:	features.tex
		pdflatex features.tex
