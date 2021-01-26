\begin{comment}
\begin{code}
module TypeVariables where

import BasicInference()
import qualified Data.Map as M
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
  show (EFunc str expr typ) =
    let
      funStr =  "(\\" ++ str ++ " -> " ++ show expr ++ ")"
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

However, the \texttt{Show} instance of the type will be different,
since there is an added case for the type variable.

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
is more interesting, because it has to
deal with type variables.  Only the added cases
for type variables are shown.

\begin{code}
unify :: Type -> Type -> Either String Type
\end{code}

\begin{comment}
\begin{code}
unify Unknown t = Right t
unify t Unknown = Right t
\end{code}
\end{comment}

\begin{code}
unify (TVar _) t = Right t
unify t (TVar _) = Right t
\end{code}

\begin{comment}
\begin{code}
unify (TFunc a b) (TFunc c d) =
  TFunc <$> (unify a c) <*> (unify b d)
unify s t =
  if s == t
  then Right s
  else Left $ "Error: could not match type " ++ show s ++ " with " ++ show t 
\end{code}
\end{comment}


The only case that changes for \texttt{infer} is the
function case and the application, because there is now the option of
giving the type \texttt{a -> a} or \texttt{a -> b} to a function. It
is not necessary to consider type variables for variable
expressions, since the type of a variable must be
determined somewhere by a literal.
The inference cases for literals are also the same
as the previous ones.

Additionally, the type environment now much include
information about type variables.  Type variables are
only defined within the context of a specific function,
not for the entire program.
\begin{code}
data TypeEnv = TypeEnv (M.Map String Type) (M.Map String Type)
\end{code}

\begin{comment}
\begin{code}

infer :: TypeEnv -> Expression -> Either String Type
infer _ (IntLiteral _ typ) = unify TInteger typ
infer _ (BoolLiteral _ typ) = unify TBool typ
infer _ (StringLiteral _ typ) = unify TString typ
infer (TypeEnv env _) e@(Var v typ) =
  case M.lookup v env of
    Nothing -> Left ("Variable not in scope: " ++ show e)
    Just t -> unify t typ
\end{code}
\end{comment}

To infer the type of a function, we get new type variables
and assign those to the argument and output of the function.
After unification, those types might still be variables,
or they might be a specific type.  Otherwise the inference continues
in the same manner as before.
\begin{code}
infer (TypeEnv typs vars) e@(EFunc var expr typ) =
  let
    nextVar :: String -> String
    nextVar [] = error "nextVar: should not be called on an empty string"
    nextVar vr@(v:vs) =
      case M.lookup vr vars of
        Nothing -> vr
        _ -> nextVar ((succ v):vs)
    var1 = nextVar "a"
    var2 = nextVar var1
  in
  case unify typ (TFunc (TVar var1) (TVar var2)) of
    Right (TFunc fin fout) ->
      let
        newVars = M.insert var1 fin $ M.insert var2 fout vars
        newTypes = M.insert var fin typs
      in
      TFunc fin <$> (infer (TypeEnv newTypes newVars) expr)
    _ -> Left $ "Not a function: " ++ show e
\end{code}

Similarly, for the application of a function, the argument to the
function might indicate that the output type is more specific, so
we keep track of that in the type variable environment.
The type variable environment becomes useful here for storing
the new specific type of the variable and retrieving it later for
use as the output type.
First this function finds separately the types of the function
and of the argument.  After checking the compatibility of the input
type, it checks if the argument made the function more specific.
Note that the type of the function remains unchanged by applying
it to a more specific argument.  For example, a function of type
\texttt{a -> a} still has the same type, even after being applied
to an integer.  Later on the program, it could be applied to a boolean.
The more specific type of the function only exists for a particular
application.
Finally, it unifies the given argument with the expected output from
the function.
\begin{code}
infer env@(TypeEnv _ tvars) (Application e1 e2 typ) = do
  e1type <- infer env e1
  (TFunc fin fout) <- unify e1type (TFunc Unknown typ)
  e2type <- infer env e2
  inType <- unify e2type fin
  let tnew = case fin of
            TVar v -> M.insert v inType tvars
            _ -> tvars
  case fout of
    TVar v ->
      case M.lookup v tnew of
        Just t -> unify typ t
        Nothing -> unify typ fout
    _ -> unify typ fout
\end{code}



Here is the code which runs the examples given below.
It prints out each example and then the type of the
entire example.

\begin{comment}
\begin{code}
main :: IO ()
main = do
  let example2 = IntLiteral 5 TBool
  let test2 = infer (TypeEnv M.empty M.empty) example2
  putStr "Example 2:  "
  print example2
  putStr "Type:       "
  print test2
\end{code}
\end{comment}

\begin{enumerate}
\item Again, we have our example of a identity function which
takes and returns the same type.  However, this time, \texttt{infer}
recognizes that the input and output types must be the same, and
gives them the same type variables.
\begin{code}
  let example11 = EFunc "x" (Var "x" Unknown) Unknown
  let test11 = infer (TypeEnv M.empty M.empty) example11
\end{code}

\begin{comment}
\begin{code}
  putStr "Example 11:  "
  print example11
  putStr "Type:        "
  print test11
\end{code}
\end{comment}


\item This is again another function example from before,
which now needs a type variable.
\begin{code}
  let example12 = EFunc "x" (IntLiteral 5 Unknown) Unknown
  let test12 = infer (TypeEnv M.empty M.empty) example12
\end{code}


\begin{comment}
\begin{code}
  putStr "Example 12:  "
  print example12
  putStr "Type:        "
  print test12
\end{code}
\end{comment}

\item This is the example that caused trouble before.  Now
it can tell that the output must be an integer, since the argument
is a \texttt{TInteger}.
\begin{code}
  let example13 = Application (EFunc "x" (Var "x" Unknown) Unknown) (IntLiteral 10 Unknown) Unknown
  let test13 = infer (TypeEnv M.empty M.empty) example13
\end{code}
\begin{comment}
\begin{code}
  putStr "Example 13:  "
  print example13
  putStr "Type:        "
  print test13
\end{code}
\end{comment}


\item Making a function of two arguments in this toy language
requires making a function which returns another function.  In
this case, the function looks like:
\begin{verbatim}
(\x -> (\y -> x))
\end{verbatim}
in which \texttt{x} and \texttt{y} are both arguments.
\begin{code}
  let example14 = EFunc "x" (EFunc "y" (Var "x" Unknown) Unknown) Unknown
  let test14 = infer (TypeEnv M.empty M.empty) example14
\end{code}


\begin{comment}
\begin{code}
  putStr "Example 14:  "
  print example14
  putStr "Type:        "
  print test14
\end{code}
\end{comment}



\end{enumerate}


% let-polymorphic is something that you might come across in reading
% it's only really relevent when polymorphism is available
% (e.g.) f :: a -> a, where a could be any type
% this is not the case in our toy language, so don't worry about it for now
% if there's time later, we'll get to this

