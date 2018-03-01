all: ExecSummary.pdf

ExecSummary: ExecSummary.lhs
	ghc --make ExecSummary.lhs

ExecSummary.pdf: ExecSummary.lhs
	pdflatex ExecSummary.lhs
	bibtex ExecSummary
	pdflatex ExecSummary.lhs
	pdflatex ExecSummary.lhs

ExecSummary.tex: ExecSummary.lhs
	lhs2TeX ExecSummary.lhs > ExecSummary.tex

clean:
	rm -f *tex *aux *log *out *ptb *~ *dvi *hi *o ExecSummary