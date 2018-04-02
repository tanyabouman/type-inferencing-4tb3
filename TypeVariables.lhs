\begin{comment}
\begin{code}
import qualified Data.Map as M
\end{code}
\end{comment}


\begin{code}
data Type = TString
          | TInteger
          | TBool
          | TVar String
          | Unknown
          | TFunc Type Type

\end{code}
