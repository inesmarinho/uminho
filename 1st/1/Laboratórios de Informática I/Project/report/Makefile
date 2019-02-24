PROJECT  = report
OPTIONS  = -shell-escape -interaction=nonstopmode -file-line-error
TEX      = pdflatex
BIBTEX   = bibtex references
BUILDTEX = $(TEX) $(OPTIONS) $(PROJECT).tex

build:
	$(BUILDTEX)
	$(BIBTEX)
	$(BUILDTEX)
	$(BUILDTEX)

clean-all:
	@echo "Cleaning..."
	@cat ~/.spells/art/maid.ascii
	@-rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idx *.ps *.eps *.pdf *.toc *.out *~
	@echo "...✓ done!"

clean:
	@echo "Cleaning..."
	@cat ~/.spells/art/maid.ascii
	@-rm -f *.log *.bak *.aux *.bbl *.blg *.idx *.toc *.lof *.out *~
	@echo "...✓ done!"
