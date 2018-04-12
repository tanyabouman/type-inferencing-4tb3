
\begin{comment}
\begin{code}
import qualified Data.Map as M
import Data.Either
\end{code}


\end{comment}


\begin{code}
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

Here is code that pretty prints code from the syntax tree.
Pretty printing the language is necessary in order to give proper errors that
relate back to the actual code.
While the pretty print might not exactly match the program text,
it will be close enough that the programmer can relate back to what
they wrote.  This also helps us for debugging, since it is
easier to read than the expression type.
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
\section{Basic Type Inference}


This is inference with \M.  First we start with a helper function,
\texttt{unify}.
It would be a more interesting function if there were
type variables,
but for now, it just checks that the two types match.
\begin{code}
unify :: Type -> Type -> Either String Type
unify Unknown t = Right t
unify t Unknown = Right t
unify (TFunc a b) (TFunc c d) =
  TFunc <$> (unify a c) <*> (unify b d)
unify s t =
  if s == t
  then Right s
  else Left $ "Error: could not match type " ++ show s ++ " with " ++ show t
\end{code}

Next, we get to the actual inference algorithm, as described by
Lee and Yi. \cite{Lee:1998:PFL:291891.291892}  In order to do inference
with variables, we keep track of these in a basic symbol table which
maps from the name of the variable to its type.  This allows the
definition of a variable in one place in the code and its use elsewhere.
The process is similar for booleans and strings.
\begin{code}
type TypeEnv = M.Map String Type
\end{code}

First we infer the types of constant literals.  These are quite simple.
For an integer literal, if the type signature given is an integer or
if there is no type
signature, we simply return integer as the type.
Otherwise there is an error.
\begin{code}
infer :: TypeEnv -> Expression -> Either String Type
infer env e@(IntLiteral i typ) = unify TInteger typ
infer env e@(BoolLiteral b typ) = unify TBool typ
infer env e@(StringLiteral s typ) = unify TString typ
\end{code}


Next we move on to the variable case.
If the variable is defined and the type matches the current expected type,
then we return that matching type.
If the variable is not defined or the type does not match, there is an
error.
\begin{code}
infer env e@(Var v typ) =
  case M.lookup v env of
    Nothing -> Left ("Variable not in scope: " ++ show e)
    Just t -> unify t typ
\end{code}

Inferencing on functions is more interesting because the argument of the
function needs to be made available in the type environment for the
body of the function.

\begin{code}
infer env e@(EFunc v exp typ) =
  case unify typ (TFunc Unknown Unknown) of
    Right (TFunc fin fout) -> 
      TFunc fin <$> (infer (M.insert v fin env) exp)
    _ -> Left $ "Not a function: " ++ show e
\end{code}


For application inference, we find the type of the function and
the argument, and then check that these two work together properly.
\begin{code}
infer env e@(Application e1 e2 typ) = do
  e1type <- infer env e1
  e2type <- infer env e2
  (TFunc fin fout) <- unify e1type (TFunc Unknown typ)
  inType <- unify e2type fin
  unify typ fout
\end{code}







\begin{comment}
Here is the code which runs the examples given below.
It prints out each example and then the type of the
entire example.

\begin{code}
main :: IO ()
main = do
\end{code}
\end{comment}

Here are examples.

\begin{enumerate}

\item A literal `5' with type Int.

\begin{code}
  let example1 = IntLiteral 5 TInteger
  let test1 = infer M.empty example1
\end{code}
Unsurprisingly, this returns the type \texttt{TInteger}, since both the
value of the literal and the type signature of the literal indicate
that it is an integer.
\begin{comment}
\begin{code}
  putStr "Example 1:  "
  print example1
  putStr "Type:       "
  print test1
\end{code}
\end{comment}

\item A literal `5' with type Bool.
\begin{code}
  let example2 = IntLiteral 5 TBool
  let test2 = infer M.empty example2
\end{code}
This, on the other hand, produces an error, because the value
`5' cannot have type \texttt{TBool}.
\begin{comment}
\begin{code}
  putStr "Example 2:  "
  print example2
  putStr "Type:       "
  print test2
\end{code}
\end{comment}

\item Now we move beyond checking whether or not the type signature is
correct, to inferring a type when the signature is missing. Lets check with a
literal `5' and type Unknown
\begin{code}
  let example3 = IntLiteral 5 Unknown
  let test3 = infer M.empty example3
\end{code}
It produces the type TInteger which is the correct type for the literal 5
\begin{comment}
\begin{code}
  putStr "Example 3:  "
  print example3
  putStr "Type:       "
  print test3
\end{code}
\end{comment}

\item
Also, let's consider inferring a type when we have a bool literal 'True' with type Unknown
\begin{code}
  let example4 = BoolLiteral True Unknown
  let test4 = infer M.empty example4
\end{code}
As before, here it produces the type TBool which is the correct type for the literal True
\begin{comment}
\begin{code}
  putStr "Example 4:  "
  print example4
  putStr "Type:       "
  print test4
\end{code}
\end{comment}

\item
Lastly in inferring a type, we can consider when we have a String literal with type Unknown
\begin{code}
  let example5 = StringLiteral "Hello world" Unknown
  let test5 = infer M.empty example5
\end{code}
It produces the type TString which is the correct type for the string literal ``Hello World''
\begin{comment}
\begin{code}
  putStr "Example 5:  "
  print example5
  putStr "Type:       "
  print test5
\end{code}
\end{comment}

\item
A string literal ``Hi there'' with type Bool
\begin{code}
  let example6 = StringLiteral "Hi there" TBool
  let test6 = infer M.empty example6
\end{code}
As expected this would produce an error because we have a string literal which is of type TString instead of TBool
\begin{comment}
\begin{code}
  putStr "Example 6:  "
  print example6
  putStr "Type:       "
  print test6
\end{code}
\end{comment}


\item
A function with type Unknown.  The Unknown type is where
the type variables will become useful, since having Unknown types
could be dangerous.
\begin{code}
  let example7 = EFunc "x" (BoolLiteral False Unknown) Unknown
  let test7 = infer M.empty example7
\end{code}
\begin{comment}
\begin{code}
  putStr "Example 7:  "
  print example7
  putStr "Type:       "
  print test7
\end{code}
\end{comment}



\item
A function application with type Unknown.
Since the function body is the boolean
false, the output type is false.
\begin{code}
  let example8 = Application example7 (IntLiteral 5 Unknown) Unknown
  let test8 = infer M.empty example8
\end{code}
\begin{comment}
\begin{code}
  putStr "Example 8:  "
  print example8
  putStr "Type:       "
  print test8
\end{code}
\end{comment}



\item The function here has the type \texttt{Unknown -> Unknown}.
Since we are not yet supporting type variables,
this result does not tell us that the two \texttt{Unknown}'s
are the same, so further type inferencing in the next step
will not tell us that the result of the application is an
\texttt{Int}.
\begin{code}
  let example9 = EFunc "x" (Var "x" Unknown) Unknown
  let test9 = infer M.empty example9
\end{code}
\begin{comment}
\begin{code}
  putStr "Example 9:  "
  print example9
  putStr "Type:       "
  print test9
\end{code}
\end{comment}

\item Here is where the trouble begins.  Since \texttt{infer} does
not know if the two Unknowns are the same or not, it is unable to
determine that the result type should be an integer.  This leads
us to the next section, where we add type variables to that language,
so that we can check whether or not these two \texttt{Unknown} types
must be the same.  This returns \texttt{Unknown}, but it should be
a \texttt{TInteger}, since the input and output both have the type
of the input, 10.
\begin{code}
  let example10 = Application example9 (IntLiteral 10 Unknown) Unknown
  let test10 = infer M.empty example10
\end{code}
\begin{comment}
\begin{code}
  putStr "Example 10:  "
  print example10
  putStr "Type:       "
  print test10
\end{code}
\end{comment}

\end{enumerate}



