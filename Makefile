all: ExecSummary.pdf

BasicInference: src/BasicInference.lhs
	stack build
#	ghc --make BasicInference.lhs

TypeVariables: src/TypeVariables.lhs
	stack build
#	ghc --make TypeVariables.lhs

ExecSummary.pdf: ExecSummary.lhs
	pdflatex ExecSummary.lhs
	bibtex ExecSummary
	pdflatex ExecSummary.lhs
	pdflatex ExecSummary.lhs

ExecSummary.tex: ExecSummary.lhs
	lhs2TeX ExecSummary.lhs > ExecSummary.tex

report.pdf: report.lhs src/BasicInference.lhs src/TypeVariables.lhs
	pdflatex report.lhs
	bibtex report
	pdflatex report.lhs
	pdflatex report.lhs

clean:
	rm -f *tex *aux *log *out *ptb *~ *dvi *hi *o ExecSummary
