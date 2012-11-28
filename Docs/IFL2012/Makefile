


all: full

quick: 
	pdflatex paper

full: 
	pdflatex paper
	pdflatex paper
	bibtex  paper
	pdflatex paper


plain:
	touch disable_peanut_gallery.tex
	${MAKE} clean
	${MAKE} all
	rm -f disable_peanut_gallery.tex

clean:
	rm -f ${NAME}.ps ${NAME}.pdf ${NAME}.dvi ${NAME1}.dvi *.bbl *.aux \
	*.log *.blg *.ascii *.out
