all:	pdf

pdf: 
		@pdflatex rapport
		@bibtex rapport
		@pdflatex rapport
		@pdflatex rapport

affiche: 
		@make pdf
		@evince rapport.pdf &

clean:
		@echo "Cleaning files..."
		@rm -f *.out    
		@rm -f *.aux
		@rm -f *.bbl
		@rm -f *.blg
		@rm -f *.dvi
		@rm -f *.log
		@rm -f *.out
		@rm -f *.ps
		@rm -f *.toc
		@rm -f *.nav
		@rm -f *.snm
		@rm -f *.synctex.gz
		@rm -f *.backup
		@rm -f *~
		@rm -f *.fls
		@rm -f *.pdf
		@rm -f *.fdb_latexmk


