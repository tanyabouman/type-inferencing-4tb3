\documentclass[10pt]{amsart}

\usepackage{comment}         %for block comments \begin{comment}...\end{comment}
\usepackage{geometry}
\usepackage{algpseudocode}
\usepackage{algorithm}
\usepackage{titletoc}        %for making a list of the sections; not used yet
\usepackage{listingsutf8}    %package for code environment; use this instead of verbatim to get automatic line break; use this instead of listings to get (•)
\lstnewenvironment{code}     %defining the code environment
  {\lstset{
    language=Haskell,
    basicstyle=\scriptsize\ttfamily,
    breaklines=true,             % for code to break at end of line
    literate={•}{{$\bullet$}}1,  % defining the bullet
    }}
  {}

\begin{document}

\title{Type Inferencing for Functional Programming Languages}
\author{Emily Ashworth, Tanya Bouman, Tonye Fiberesima \\ ashworel, boumante, fiberet}
\maketitle


% I'm just taking section from the description pdf
\section{High Level Description} This project is about compilers and compiler construction for functional programming languages, written in a functional programming language.

\section{Area of Focus} In particular, we are focusing on type inferencing for a toy language.  This toy language features Haskell-like syntax, but has a simpler type system, featuring only Ints, Bools, Strings and Functions.


\section{Implementation}  Similar to Martin Grabm{\"u}ller's paper \cite{algwstepbystep}, we would like to explain type inferencing with an example algorithm.
Since Grabm{\"u}ller uses Algorithm W to explain step-by-step, we would like to use Algorithm M, and perform a similar functionality.
In addition to the content explained in his paper, we also need to explain the background on type inferencing and would include some of the formal specifications of the Hindley-Milner type inferencing algorithms.


\section{Relevance to the Field}  This subject is important because while in many languages, such as C and Java, all types must be explicitly declared, this is not the case for all languages.
In many functional languages, such as Haskell, it is not necessary to specify all types, since the compiler can infer them.  A common system to accomplish this is the Hindley-Milner type inference \cite{Damas:1982:PTF:582153.582176}, which has two main algorithms, Algorithm W and Algorithm M \cite{Lee:1998:PFL:291891.291892}.


\bibliography{bib.bib}
\bibliographystyle{ieeetr}


\end{document}
