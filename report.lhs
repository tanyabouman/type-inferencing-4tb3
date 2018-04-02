\documentclass[12pt]{article}
\usepackage{cite}
\usepackage{url}
\usepackage{hyperref} % for inserting the link
\usepackage{calculation}
\usepackage{verbatim} % for multiline comments
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{color}
\usepackage{enumitem} % for alpha enumerations
\usepackage{listings} % for putting code snippets
\lstset{
  breaklines=true % line breaks for listings
}
\usepackage{cancel} % for crossing out cancellations in math

\lstnewenvironment{code}     %defining the code environment
  {\lstset{
    language=Haskell,
    basicstyle=\small\ttfamily,
    breaklines=true,             % for code to break at end of line
    literate={•}{{$\bullet$}}1,  % defining the bullet
    }}
  {}

\begin{comment}
\begin{code}
import qualified Data.Map as M
\end{code}
\end{comment}

\newcommand{\M}{$\mathcal{M}$}
\newcommand{\W}{$\mathcal{W}$}

\title{Type Inferencing in Functional Languages} 
\author{Emily Ashworth, Tanya Bouman, Tonye Fiberesima \\ 
001402976, 001416669, 001231043 \\ 
ashworel, boumante, fiberet}
\date{April 9, 2018}

\begin{document}

\maketitle

\section{Introduction}
Many advanced languages, such as Swift\cite{swifttypedocs} and Haskell\cite{haskelltypedocs},
allow programmers to skip defining the types of variables,
by doing type inferencing at compile time.
Others, like Python, also allow the programmer to avoid defining
the type of a variable, but these types are dynamic\cite{pythonsummary}, 
and therefore not the topic of the currrent report.
Languages that do type inferencing use the Hindley-Milner type system, which has two main implementations, Algorithm W and Algorithm M.

\subsection{History}
Algorithm W is standard algorithm, first done by ( somebody ), while Algorithm M was not formally presented 
until 1998 by Oukseh Lee and Kwangkeun Yi\cite{Lee:1998:PFL:291891.291892}. Algorithm W is an algorithm that works bottom to top.
This means that if a syntax tree were built from a piece of code, types would be inferred starting from the bottom. Algorithm M
is the exact inverse of Algorithm W. Types are inferred from the top of the syntax tree down.


\section{Our Work}
Because Algorithm W is the more popular and more commonly used algorithm, we decided to work with Algorithm M. To do our work, we
have created a toy language, and rules for that language, that demonstrate type inferencing and Algorithm M.

\subsection{Evaluation Rules}

\subsection{Our Toy Language}
Let's assume that a parser has already produced a syntax tree, of type
\texttt{Expression}, as given below.
The \texttt{Type} that is given from the parser is the type given in the
annotation.  The annotation might or might not be correct. The type
inferencing algorithm will detect that.



\input{BasicInference.lhs}

\input{TypeVariables.lhs}

% swift type inferencing description; maybe useful as a reference??? https://github.com/apple/swift/blob/master/docs/TypeChecker.rst

\section{Discussion}

\section{Conclusion}


\bibliographystyle{ieeetr}
\bibliography{bib.bib}

\end{document}
