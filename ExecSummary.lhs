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
\author{Emily Ashworth, Tanya Bouman, Tonye Fiberesima} 
\maketitle


% I'm just taking things from the description pdf
\section{High Level Description} This project is about compilers and complier construction for functional programming languages, written in a functional programming language.

\section{Area of Focus} In particular, we are focusing on type inferencing system for a toy language. 


\section{Implementation}  Similar to Martin Grab{\"u}ller's paper (cite https://github.com/wh5a/Algorithm-W-Step-By-Step), we would like to explain type inferencing with an example algorithm.  
Since Grab{\"u}ller uses Algorithm W to explain step-by-step, we would like to use Algorithm M, and perform a similar functionality.  
In addition to the content explained in his paper, we also need to explain the background on type inferencing and would include some of the formal specifications of type inferencing algorithms.


\section{Relevance to the Field}  This subject is important because while in many languages, such as C and Java, all types must be explicitly declared, this is not the case for functional languages.  
In many functional languages, such as Haskell, it is not necesary to specify all types, since the compiler can infer them.  A common system to accomplish this is the Hindley-Milner type inference, which has two main algorithms, Algorithm W and Algorithm M.  (talk about the differences between the two.  Cite the papers listed here https://www.quora.com/What-are-the-differences-between-Algorithm-W-and-Algorithm-M) 




\end{document}
