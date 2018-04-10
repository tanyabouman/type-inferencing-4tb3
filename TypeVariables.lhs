\begin{comment}
\begin{code}
import qualified Data.Map as M
\end{code}

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

Now, we continue on again with \texttt{unify}
and \texttt{infer}.  This time \texttt{unify}
is much more interesting, because it has to
deal with type variables.  Only the added cases
for type variables are shown.

\begin{code}
unify :: Type -> Type -> Either String Type
\end{code}

\begin{comment}
\begin{code}
unify (TVar s) t = Right t
unify t (TVar s) = Right t
unify (TFunc a b) (TFunc c d) =
  TFunc <$> (unify a c) <*> (unify b d)
unify s t =
  if s == t
  then Right s
  else Left $ "Error: could not match type " ++ show s ++ " with " ++ show t 
\end{code}
\end{comment}


% FIXME: sort this out after the inference algorithm
\begin{code}


\end{code}

The only case that changes for \texttt{infer} is the
function case, because there is now the option of
giving the type \texttt{a -> a} to a function. It
is not possible to use type variables for variable
expressions, since the type of a variable must be
determined somewhere by a literal.
The inference cases for literals are the same
as the previous ones.
\begin{comment}
\begin{code}
type TypeEnv = M.Map String Type

infer :: TypeEnv -> Expression -> Either String Type
infer env e@(IntLiteral i typ) = unify TInteger typ
infer env e@(BoolLiteral b typ) = unify TBool typ
infer env e@(StringLiteral s typ) = unify TString typ
-- NOTE: include in Basic Inference that the parser
-- would already have caught this error
infer env e@(Var v typ) =
  case M.lookup v env of
    Nothing -> Left ("Variable not in scope: " ++ show e)
    Just t -> unify t typ
\end{code}
\end{comment}


\begin{code}
infer env e@(EFunc v exp typ) = 
  case unify typ (TFunc (TVar "a") (TVar "b")) of
    Right (TFunc fin fout) ->
      TFunc fin <$> (infer (M.insert v fin env) exp)
    _ -> Left $ "Not a function: " ++ show e
\end{code}



Here is the code which runs the examples given below.
It prints out each example and then the type of the
entire example.

\begin{comment}
\begin{code}
main :: IO ()
main = do
  let example2 = IntLiteral 5 TBool
  let test2 = infer M.empty example2
  putStr "Example 2:  "
  print example2
  putStr "Type:       "
  print test2
\end{code}
\end{comment}

Here are examples.
\begin{enumerate}
\item
\begin{code}
  let example15 = EFunc "x" (Var "x" Unknown) Unknown
  let test15 = infer M.empty example15
\end{code}

\begin{comment}
\begin{code}
  putStr "Example 15:  "
  print example15
  putStr "Type:        "
  print test15
\end{code}
\end{comment}


\item
\begin{code}
  let example16 = EFunc "x" (IntLiteral 5 Unknown) Unknown
  let test16 = infer M.empty example16
\end{code}


\begin{comment}
\begin{code}
  putStr "Example 16:  "
  print example16
  putStr "Type:        "
  print test16
\end{code}
\end{comment}


\end{enumerate}


% let-polymorphic is something that you might come across in reading
% it's only really relevent when polymorphism is available
% (e.g.) f :: a -> a, where a could be any type
% this is not the case in our toy language, so don't worry about it for now
% if there's time later, we'll get to this

