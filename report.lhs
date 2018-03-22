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

\title{4TB3 } 
\author{Emily Ashworth, Tanya Bouman, Tonye Fiberesima \\ 
001402976, 001416669, 001231043 \\ 
ashworel, boumante, fiberet}
\date{April 9, 2018}

\begin{document}

\maketitle

\section{Introduction}
% what is type inferencing?
% history

Many advanced languages, such as Swift\cite{swifttypedocs} and Haskell\cite{haskelltypedocs},
allow programmers to skip defining the types of variables,
by doing type inferencing at compile time.
Others, like Python, also allow the programmer to avoid defining
the type of a variable, but these types are dynamic\cite{pythonsummary}, 
and therefore not the topic of the currrent report.

These languages use the Hindley-Milner type system, which has two main implementations, Algorithm W and Algorithm M.  (cite something)  Algorithm W is standard algorithm, first done by ( somebody ), while Algorithm M was not formally presented until 1998 by Oukseh Lee and Kwangkeun Yi\cite{Lee:1998:PFL:291891.291892}.

% explain the difference between M and W






\section{Our Toy Language}

%FIXME: should the type be included in these data types?
% there should be a type included, because there might have been
% a type annotation.

Let's assume that a parser has already produced a syntax tree, of type
\texttt{Expression}, as given below.
The \texttt{Type} that is given from the parser is the type given in the
annotation.  The annotation might or might not be correct. The type
inferencing algorithm will detect that.
\begin{code}

-- these are the currently available types;
-- this might be expanded
data Type = TString
          | TInteger
          | TBool
          | Unknown
          | TFunc Type Type
          deriving (Eq, Ord, Show)

data Expression = Var String Type
                | IntLiteral Int Type
                | StringLiteral String Type
                | BoolLiteral Bool Type
                | EFunc String Expression Type
                | Application Expression Expression Type
                deriving (Eq, Ord, Show)
\end{code}

Pretty printing the language is necessary in order to give proper errors that
relate back to the actual code.

\begin{code}

\end{code}

% as we explain how the algorithm works, make the code
\section{Code}

Here is an example.
\begin{code}



\end{code}

After we call \texttt{infer} on \texttt{program}, the results is a mapping
from the expressions to their types.

This is inference with \M

Variables should not be a separate thing from functions, because
we don't yet know if the types will be Int or Int -> Int or whatever.

\begin{code}

\end{code}

This would be a more interesting function if there were type variables,
but for now, it just checks that the two types match.
\begin{code}
unify :: Type -> Type -> Maybe Type
unify Unknown t = Just t
unify t Unknown = Just t
unify (TFunc a b) (TFunc c d) =
  TFunc <$> (unify a c) <$> (unify b d)
unify s t =
  case s of
    t -> Just s
    _ -> Nothing
\end{code}


First we infer the types of constant literals.  These are quite simple.
If the type signature given is an integer or if there is no type
signature, we simply return integer as the type.
Otherwise there is an error.
% maybe include an example of how to get this error
\begin{code}
type TypeEnv = M.Map String Type

infer :: TypeEnv -> Expression -> Type
infer _ e@(IntLiteral i typ) =
  case unify TInteger typ of
    Nothing -> error ("Type mismatch: " ++ show e)
    Just i -> i

-- do the same for the other constants
\end{code}


Next we move on to the variable case.
If the variable is defined and the type matches the current expected type,
then we return that matching type.
If the variable is not defined or the type does not match, there is an
error.
\begin{code}
infer env e@(Var v typ) =
  case M.lookup v env of
    Nothing -> error ("Variable not in scope: " ++ show e)
    Just t ->
      case unify t typ of
        Nothing -> error ("Type mismatch: " ++ show e)
        Just s -> s
\end{code}

Inferencing on functions is more interesting because the argument of the
function needs to be made available in the context for the body of the
function.

\begin{code}
infer env e@(EFunc v exp typ) =
  case unify typ (TFunc Unknown Unknown) of
    Nothing -> error ("Type mismatch: " ++ show e)
    Just (TFunc fin fout) -> TFunc fin (infer (M.insert v fin env) e)
\end{code}


\begin{code}
infer env e@(Application e1 e2 typ) =
  case M.lookup e1 env of
    Just (TFunc fin fout) ->
      case unify fout typ of
        Just o -> infer env e2 fin
        Nothing -> error ("Type mismatch: " ++ show e1 ++ " and " ++ show e2)
    _ -> error ("Tried to apply: " ++ show e1 ++ " to variable " ++ show e2 ++ " when " ++ show e1 ++ " is not a function")
\end{code}



% let-polymorphic is something that you might come across in reading
% it's only really relevent when polymorphism is available 
% (e.g.) f :: a -> a, where a could be any type
% this is not the case in our toy language, so don't worry about it for now
% if there's time later, we'll get to this



% swift type inferencing description; maybe useful as a reference??? https://github.com/apple/swift/blob/master/docs/TypeChecker.rst


\bibliographystyle{ieeetr}
\bibliography{bib.bib}

\end{document}
