PDFC=pdflatex

pdf: slides.tex slides.bib
	pdflatex slides -draftmode
	bibtex slides
	pdflatex slides
	
clean:
	$(RM) *.aux *.lof *.log *.lot *.toc *.bbl *.blg *pdf *.ml *.dvi *-blx.bib *.xml *.out *.nav *.snm *.vrb
