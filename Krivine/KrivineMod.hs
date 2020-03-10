{- Implementation of Krivine’s K-Machine extended to handle an applied Λ-calculus -}
type Env = [Expr]

data Expr
  = Index Int
  | Application Expr Expr
  | Lambda Expr
  | Closure Env Expr
  | Primitive Value
  deriving (Show)

data Value
  = Function (Value -> Value)
  | Int Integer

instance Show Value where
  show (Function _) = "Function"
  show (Int i)      = "Int " ++ show i

type E = Env -- Environment

type T = Expr -- value to be transformed

type S = [Expr] -- working stack for closures

data State
  = WeakNormalForm Expr
  | ToDo (E, T, S)
  deriving (Show)

krivineEval :: (E, T, S) -> State
krivineEval (Closure env expr:_, Index 0, s) = ToDo (env, expr, s)
krivineEval (_:env, Index i, s)
  | i > 0 = ToDo (env, Index (i - 1), s)
krivineEval (env0, Primitive (Function f), Closure env1 expr1:s) =
  ToDo (env', Primitive $ f operand, s)
  where
    env' =
      case env0 of
        (_:e) -> e
        []    -> []
    operand =
      case eval' (env1, expr1, []) of
        (Primitive v) -> v
        operand       -> error $ "Invalid operand: " ++ show operand
krivineEval (env, Application expr0 expr1, s) =
  ToDo (env, expr0, Closure env expr1 : s)
krivineEval (env, Lambda expr0, s:ss) = ToDo (s : env, expr0, ss)
krivineEval ([], expr, []) = WeakNormalForm expr
krivineEval x = error $ "\nInvalid State:\n" ++ show x ++ "\n\n"

eval' ets -- reduce an expression to weak normal form
 =
  case krivineEval ets of
    WeakNormalForm e -> e
    ToDo ets         -> eval' ets

eval :: Expr -> Expr
eval e = eval' ([], e, [])

primitiveAdd :: Value -> Value
primitiveAdd (Int a) = Function f
  where
    f (Int b) = Int $ a + b
    f b       = error $ "Invalid second argument to add: " ++ show b
primitiveAdd a = error $ "Invalid first argument to add: " ++ show a

testProgram =
  Application (Lambda (Application (Index 0) (Index 0))) (Lambda (Index 0))

testAdd =
  Application
    (Application (Primitive $ Function primitiveAdd) (Primitive $ Int 5))
    (Primitive $ Int 2)

testABitMoreComplex -- (λa.λb.b) 2 ((λa.λb.+ a b) 5 ((λa.λb.b) 5 6)) -β-> 11
 =
  Application
    (Application snd (Primitive $ Int 2))
    (Application
       (Application add (Primitive $ Int 5))
       (Application (Application snd (Primitive $ Int 5)) (Primitive $ Int 6)))
  where
    add =
      Lambda $
      Lambda $
      Application
        (Application (Primitive $ Function primitiveAdd) (Index 1))
        (Index 0)
    snd = Lambda $ Lambda $ Index 0

main = do
  print $ eval testProgram -- should evaluate to Lambda (Index 0)
  print $ eval testAdd -- should evaluate to Primitive (Value 7)
  print $ eval testABitMoreComplex -- should evaluate to Primitive (Value 11)
