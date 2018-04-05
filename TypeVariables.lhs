\begin{comment}
\begin{code}
import qualified Data.Map as M
\end{code}
\end{comment}

\section{Inference with Type Variables}

In order to inference with type variables,
we add the option \texttt{TVar} to the
type.

\begin{code}
data Type = TString
          | TInteger
          | TBool
          | TVar String
          | Unknown
          | TFunc Type Type

\end{code}
