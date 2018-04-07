all: ExecSummary.pdf

BasicInference: BasicInference.lhs
	ghc --make BasicInference.lhs

TypeVariables: TypeVariables.lhs
	ghc --make TypeVariables.lhs

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