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
\linespread{1.25} % for 1.5 spacing

\maketitle

\section{Introduction}
Type inferencing can be referred to as the process of analyzing type information in a program based on the use of some of its variables at compile time.\cite{duggan_bent_1996}
Many advanced languages, such as Swift\cite{swifttypedocs} and Haskell\cite{haskelltypedocs},
allow programmers to skip defining the types of variables, by doing type inferencing at compile time.
Others, like Python, also allow the programmer to avoid defining
the type of a variable, but these types are dynamic\cite{pythonsummary}, 
and therefore not the topic of the current report.
The most common type of type inferencing that languages use  is the Hindley-Milner type system, which has two main implementations, Algorithm W and Algorithm M.

\subsection{History}
The original type inference algorithm was invented by Haskell Curry and Robert Feys in 1958 for the simply typed lambda calculus. In 1968, Roger Hendley worked on extending the algorithm and proved that it always produced the most general type. In 1978, Robin Milner solely developed an equivalent algorithm called Algorithm W while working on designing ML and in 1985 Luis Damas proved that Milner's algorithm was complete and extended it to support polymorphic references.\cite{lee_2018} 
Algorithm M derived from Algorithm W was not formally presented until 1998 by Oukseh Lee and Kwangkeun Yi\cite{Lee:1998:PFL:291891.291892}. 
These algorithms are called the Hindely-Milner type inference algorithms, Algorithm W and Algorithm M. Algorithm W is the standard algorithm that works bottom to top which means that if a syntax tree were built from a piece of code, types would be inferred starting from the bottom while Algorithm M is the exact inverse of Algorithm W. Types are inferred from the top of the syntax tree down. 


\section{Our Work}
To do our work, we have created a toy language, and rules for that language, that demonstrate type inferencing with Algorithm M. We chose to use Algorithm M because Algorithm W is the more popular and more commonly used algorithm and also Algorithm M always finds type errors earlier by considering a less number of expressions than Algorithm W as proposed by Oukseh Lee and Kwangkeun Yi\cite{Lee:1998:PFL:291891.291892}. 
We would implement Algorithm  M with our toy language to demonstrate how type inferencing works in functional programming.

\subsection{Our Toy Language}

The toy language for the purpose of this project is a
small portion of Haskell, allowing only the types
\texttt{Int}, \texttt{Bool}, \texttt{String} and functions
on those types.  Later on in this report, we extend
the language to also include type variables, but these
are not constrained by type classes.

\subsection{Type Inferencing Rules}

The type inferencing rules are rules specific to the language.  These
are used with Algorithm M, Algorithm W or any other type inferencing
algorithm.  Presented here are the rules necessary to infer
types for our toy language.


\subsection{Inferencing}

Let's assume that a parser has already produced a syntax tree, of type
\texttt{Expression}, as given below.
The \texttt{Type} that is given from the parser is the type given in the
annotation.  The annotation might or might not be correct. The type
inferencing algorithm will detect that.

\input{BasicInference.lhs}

\input{TypeVariables.lhs}

% swift type inferencing description; maybe useful as a reference??? https://github.com/apple/swift/blob/master/docs/TypeChecker.rst

\section{Conclusion}

To conclude, type inferencing is a very interesting feature in the
programming world. It can use Algorithm M, Algorithm W or other similar
algorithm to infer the types of variables which are undeclared at
compile time, thus enabling the programmer to be faster.
Our example toy language shows how Algorithm M and the type inferencing system work. In the future, it would be
interesting to see if type inferencing could be applied across all languages.


\bibliographystyle{ieeetr}
\bibliography{bib.bib}

\end{document}
