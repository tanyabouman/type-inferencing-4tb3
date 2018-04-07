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

Now, we continue on again with \texttt{unify}
and \texttt{infer}.  This time \texttt{unify}
is much more interesting, because it has to
deal with type variables.  Only the added cases
for type variables are shown.

\begin{code}
unify :: Type -> Type -> Maybe Type
\end{code}

\begin{comment}
\begin{code}
unify Unknown t = Just t
unify t Unknown = Just t
unify (TFunc a b) (TFunc c d) =
  TFunc <$> (unify a c) <*> (unify b d)
\end{code}
\end{comment}


% FIXME: sort this out after the inference algorithm
\begin{code}


\end{code}

Since inference on a variable could get more
information about that variable than was already
known in the environment, we also now need to
return an updated environment.
The inference cases for literals are the same
as the previous ones.
\begin{comment}
\begin{code}
type TypeEnv = M.Map String Type

infer :: TypeEnv -> Expression -> (Type, TypeEnv)
infer env e@(IntLiteral i typ) =
  case unify TInteger typ of
    Nothing -> error $ "Type mismatch: literal " ++ show i
               ++ " cannot have type " ++ show typ
    Just i -> (i, env)
infer env e@(BoolLiteral b typ) =
  case unify TBool typ of
    Nothing -> error $ "Type mismatch: literal " ++ show b
               ++ " cannot have type " ++ show typ
    Just b -> (b, env)
infer env e@(StringLiteral s typ) =
  case unify TString typ of
    Nothing -> error $ "Type mismatch: literal " ++ show s
               ++ " cannot have type " ++ show typ
    Just s -> (s, env)
\end{code}
\end{comment}
\begin{code}

\end{code}

% let-polymorphic is something that you might come across in reading
% it's only really relevent when polymorphism is available
% (e.g.) f :: a -> a, where a could be any type
% this is not the case in our toy language, so don't worry about it for now
% if there's time later, we'll get to this

