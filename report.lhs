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
          deriving (Eq, Ord)

data Expression = Var String Type
                | IntLiteral Int Type
                | StringLiteral String Type
                | BoolLiteral Bool Type
                | EFunc String Expression Type
                | Application Expression Expression Type
                deriving (Eq, Ord)
\end{code}

Pretty printing the language is necessary in order to give proper errors that
relate back to the actual code.
% is there a benefit to separating pretty from show?
\begin{code}
instance Show Expression where
  show (IntLiteral int typ) =
    case typ of
      Unknown -> show int
      _ -> show int ++ " :: " ++ show typ
  show (StringLiteral str typ) =
    case typ of
      Unknown -> show str
      _ -> show str ++ " :: " ++ show typ
  show (BoolLiteral bl typ) =
    case typ of
      Unknown -> show bl
      _ -> show bl ++ " :: " ++ show typ
  show (Var str typ) =
    case typ of
      Unknown -> str
      _ -> str ++ " :: " ++ show typ
  show (EFunc str exp typ) =
    let
      funStr =  "(\\" ++ str ++ " -> " ++ show exp ++ ")"
    in
    case typ of
      Unknown -> funStr
      _ -> funStr ++ " :: " ++ show typ
  show (Application e1 e2 typ) =
    case typ of
      Unknown -> show e1 ++ " " ++ show e2
      _ -> "(" ++ show e1 ++ " " ++ show e2 ++ ") :: " ++ show typ

instance Show Type where
  show TString = "String"
  show TInteger = "Int"
  show TBool = "Bool"
  show Unknown = "?"
  show (TFunc t1 t2) = show t1 ++ " -> " ++ show t2
\end{code}

% as we explain how the algorithm works, make the code
\section{Code}

Here are examples.

\begin{itemize}

\item A literal `5' with type Int.
\begin{code}
example1 = IntLiteral 5 TInteger
test1 = infer M.empty example1
\end{code}
Unsurprisingly, this returns the type \texttt{TInteger}, since both the 
value of the literal and the type signature of the literal indicate
that it is an integer.

\item This, on the other hand, produces an error, because the value
`5' cannot have type \texttt{TBool}.  
\begin{code}
example2 = IntLiteral 5 TBool
test2 = infer M.empty example2
\end{code}

\item Now we move beyond checking whether or not the type signature is
correct, to infering a type when the signature is missing.

\begin{code}
example3 = IntLiteral 5 Unknown
test3 = infer M.empty example3
\end{code}

\item

\begin{code}
example4 = BoolLiteral True Unknown
test4 = infer M.empty example4
\end{code}

\item

\begin{code}
example5 = StringLiteral "Hello world" Unknown
test5 = infer M.empty example5
\end{code}

\item

\begin{code}
example6 = StringLiteral "Hi there" TBool
test6 = infer M.empty example6
\end{code}

\item
\begin{code}
example7 = EFunc "x" (BoolLiteral False Unknown) Unknown
test7 = infer M.empty example7
\end{code}


\item
\begin{code}
example8 = Application example7 (IntLiteral 5 Unknown) Unknown
test8 = infer M.empty example8
\end{code}

\item
\begin{code}
example9 = EFunc "x" (Var "x" Unknown) Unknown
test9 = infer M.empty example9
\end{code}

\item
\begin{code}
example10 = Application example9 (IntLiteral 10 Unknown) Unknown
\end{code}

\end{itemize}

This is inference with \M


\begin{code}

\end{code}

This would be a more interesting function if there were type variables,
but for now, it just checks that the two types match.
\begin{code}
unify :: Type -> Type -> Maybe Type
unify Unknown t = Just t
unify t Unknown = Just t
unify (TFunc a b) (TFunc c d) =
  TFunc <$> (unify a c) <*> (unify b d)
unify s t =
  if s == t
  then Just s
  else Nothing
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
    Nothing -> error $ "Type mismatch: literal " ++ show i 
               ++ " cannot have type " ++ show typ
    Just i -> i
infer _ e@(BoolLiteral b typ) =
  case unify TBool typ of
    Nothing -> error $ "Type mismatch: literal " ++ show b
               ++ " cannot have type " ++ show typ
    Just b -> b
infer _ e@(StringLiteral s typ) =
  case unify TString typ of
    Nothing -> error $ "Type mismatch: literal " ++ show s
               ++ " cannot have type " ++ show typ
    Just s -> s
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
    Just (TFunc fin fout) -> TFunc fin (infer (M.insert v fin env) exp)
\end{code}


\begin{code}
infer env e@(Application e1 e2 typ) =
  let
    -- infer the type of e1
    e1type = infer env e1
    e2type = infer env e2
  in
    -- unify that with what the output of the function should be
    case unify e1type (TFunc Unknown typ) of
      Just (TFunc fin fout) ->
        -- match the input type with the argument
        case unify e2type fin of
          Just t ->
            -- match the return type with the overall type
            case unify fout typ of
              Just s -> s
              Nothing -> error $ "Could not match: " ++ show fout ++ " and " ++ show typ
          Nothing -> error $ "Could not match: " ++ show e2type ++ " and " ++ show fin
      _ -> error $ "Could not match: " ++ show e1type ++ " and " ++ show typ


\end{code}



% let-polymorphic is something that you might come across in reading
% it's only really relevent when polymorphism is available 
% (e.g.) f :: a -> a, where a could be any type
% this is not the case in our toy language, so don't worry about it for now
% if there's time later, we'll get to this



% swift type inferencing description; maybe useful as a reference??? https://github.com/apple/swift/blob/master/docs/TypeChecker.rst

\section{Discussion}

\section{Conclusion}


\bibliographystyle{ieeetr}
\bibliography{bib.bib}

\end{document}
