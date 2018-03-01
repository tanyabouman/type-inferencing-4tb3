all: ExecSummary ExecSummary.pdf

ExecSummary: ExecSummary.lhs
	ghc --make ExecSummary.lhs

ExecSummary.pdf: ExecSummary.tex
	latex ExecSummary.tex
	pdflatex ExecSummary.tex

ExecSummary.tex: ExecSummary.lhs
	lhs2TeX ExecSummary.lhs > ExecSummary.tex

clean:
	rm -f *tex *aux *log *out *ptb *~ *dvi *hi *o ExecSummary