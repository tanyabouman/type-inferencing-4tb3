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

report.pdf: report.lhs
	pdflatex report.lhs
	bibtex report
	pdflatex report.lhs
	pdflatex report.lhs

clean:
	rm -f *tex *aux *log *out *ptb *~ *dvi *hi *o ExecSummary