-- Implementation of the K-Machine
type Env = [Expr]

data Expr
  = Index Int
  | Application Expr Expr
  | Lambda Expr
  | Closure Env Expr
  deriving (Show)

type E = Env -- Environment

type T = Expr -- value to be transformed

type S = [Expr] -- working stack for closures

data State
  = WeakNormalForm Expr
  | ToDo (E, T, S)

krivineEval :: (E, T, S) -> State
krivineEval (Closure env expr:_, Index 0, s) = ToDo (env, expr, s)
krivineEval (v:env, Index i, s)
  | i > 0 = ToDo (env, Index (i - 1), s)
krivineEval (env, Application expr0 expr1, s) =
  ToDo (env, expr0, Closure env expr1 : s)
krivineEval (env, Lambda expr0, s:ss) = ToDo (s : env, expr0, ss)
krivineEval ([], expr, []) = WeakNormalForm expr

eval :: Expr -> Expr
eval e = eval' ([], e, [])
  where
    eval' ets =
      case krivineEval ets of
        WeakNormalForm e -> e
        ToDo ets         -> eval' ets

testProgram =
  Application (Lambda (Application (Index 0) (Index 0))) (Lambda (Index 0))

main = print $ eval testProgram -- should evaluate to Lambda (Index 0)
