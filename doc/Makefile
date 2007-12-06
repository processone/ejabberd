# $Id$

all:	release pdf html

release:
	@echo "Notes for the releaser:"
	@echo "* Do not forget to add a link to the release notes in guide.tex"
	@echo "* Do not forget to update the version number in src/ejabberd.hrl!"
	@echo "* Do not forget to update the features in introduction.tex (including \new{} and \improved{} tags)."
	@echo "Press any key to continue"
	@read foo
	@echo "% ejabberd version (automatically generated)." > version.tex
	@echo "\\\newcommand{\\\version}{"`cat ../src/ejabberd.hrl | grep VERSION | sed s/-define\(VERSION,\ \"//g | sed s/\"\).//g`"}" >> version.tex

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
