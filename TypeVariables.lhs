\begin{comment}
\begin{code}
import qualified Data.Map as M
\end{code}

Here is the code which runs the examples given below.
It prints out each example and then the type of the
entire example.

\begin{code}
examples =
  [ ]


-- FIXME: this should be prettier
main :: IO ()
main = do
  -- sequence $ map print examples
  putStrLn "Nothing yet."
\end{code}
\end{comment}

\section{Inference with Type Variables}

In order to inference with type variables,
we add the option \texttt{TVar} to the
type. As indicated by the Lexical Structure section
of the Haskell 89 report, the type variables must all begin
with a lowercase letter.  \cite{haskell98report}
The parser takes care of the enforcement,
so we do not need to worry about this.


\begin{code}
data Type = TString
          | TInteger
          | TBool
          | TVar String
          | Unknown
          | TFunc Type Type
          deriving (Eq, Ord)
\end{code}

The expression data type is exactly the same as before, since
we do not add any new syntax.

\begin{comment}
\begin{code}
data Expression = Var String Type
                | IntLiteral Int Type
                | StringLiteral String Type
                | BoolLiteral Bool Type
                | EFunc String Expression Type
                | Application Expression Expression Type
                deriving (Eq, Ord)
\end{code}

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
\end{code}
\end{comment}

However, the \texttt{Show} instance will be different.


\begin{code}
instance Show Type where
  show TString = "String"
  show TInteger = "Int"
  show TBool = "Bool"
  show (TVar v1) = v1
  show Unknown = "?"
  show (TFunc t1 t2) = show t1 ++ " -> " ++ show t2
\end{code}

% let-polymorphic is something that you might come across in reading
% it's only really relevent when polymorphism is available
% (e.g.) f :: a -> a, where a could be any type
% this is not the case in our toy language, so don't worry about it for now
% if there's time later, we'll get to this

