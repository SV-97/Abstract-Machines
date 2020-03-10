import           Data.Char (isAlpha, isDigit)

{-
Namesless pure Λ-calculus (essentially the λ calculus with lexical adressing)

e ::= #i
    | (e0 e1)
    | Λ.e0

i ::= [0..9]+
-}
data Expr
  = Idx Integer
  | App Expr Expr
  | Lam Expr
  | Free Char
  deriving (Eq, Show)

class PShow a where
  pShow :: a -> String

instance PShow Expr where
  pShow (Idx i) = "#" ++ show i
  pShow (App v0@(Free _) i1@(Idx _)) = pShow v0 ++ " " ++ pShow i1
  pShow (App i0@(Idx _) v1@(Free _)) = pShow i0 ++ " " ++ pShow v1
  pShow (App i0@(Idx _) i1@(Idx _)) = pShow i0 ++ " " ++ pShow i1
  pShow (App i0@(Idx _) e1) = pShow i0 ++ " (" ++ pShow e1 ++ ")"
  pShow (App e0 i1@(Idx _)) = "(" ++ pShow e0 ++ ") " ++ pShow i1
  pShow (App v0@(Free _) v1@(Free _)) = pShow v0 ++ " " ++ pShow v1
  pShow (App v0@(Free _) e1) = pShow v0 ++ " (" ++ pShow e1 ++ ")"
  pShow (App e0 v1@(Free _)) = "(" ++ pShow e0 ++ ") " ++ pShow v1
  pShow (App e0 e1) = "(" ++ pShow e0 ++ ") (" ++ pShow e1 ++ ")"
  pShow (Lam e0) = "Λ." ++ pShow e0
  pShow (Free c) = c : ""

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

parseNumber :: String -> (Integer, String)
parseNumber t = (read t1, t2)
  where
    (t1, t2) = span isDigit t

fromString :: String -> (Expr, String)
fromString ('#':ts) = mapFst Idx $ parseNumber ts
fromString ('(':ts) = (App e0 e1, t2)
  where
    (e0, t0) = fromString ts
    (e1, t1) = fromString t0
    t2 = drop 1 $ dropWhile (/= ')') t1
fromString ('Λ':'.':ts) = mapFst Lam $ fromString ts
fromString (' ':ts) = fromString ts
fromString (c:ts)
  | isAlpha c = (Free c, ts)
fromString ts = error ts

-- increment all free occurences of binding indices in e
p :: Integer -> Expr -> Expr
p k (Idx i)
  | k > i = Idx i
  | k <= i = Idx (i + 1)
p k (App e0 e1) = App (p k e0) (p k e1)
p k (Lam e0) = Lam (p (k + 1) e0)
p _ e0@(Free c) = e0

-- decrement all free occurences of binding indices in e
q :: Integer -> Expr -> Expr
q k (Idx i)
  | k > i = Idx i
  | otherwise = Idx (i - 1)
q k (App e0 e1) = App (q k e0) (q k e1)
q k (Lam e0) = Lam (q (k + 1) e0)
q _ e0@(Free c) = e0

-- substitute in e0 all occurences of binding indices  at distance k by e1
s :: Integer -> Expr -> Expr -> Expr
s k e1 (Idx i)
  | i /= k = Idx i
  | i == k = e1
s k e1 (App ea eb) = App (s k e1 ea) (s k e1 eb)
s k e1 (Lam ea) = Lam (s (k + 1) (p 0 e1) ea)
s _ _ e0@(Free c) = e0

βReduce :: Expr -> Expr
βReduce (App (Lam e0) e1)     = q 0 (s 0 (p 0 e1) e0)
βReduce (App e0@(App _ _) e1) = βReduce (App (βReduce e0) e1)
βReduce e                     = e

βReduceFix :: Expr -> Expr
βReduceFix e0
  | e0 == βReduce e0 = e0
  | otherwise = βReduceFix $ βReduce e0

eval :: String -> Expr
eval = βReduce . fst . fromString

-- "(Λ.Λ.(#0 #1) #2)"
expr = App (Lam $ Lam $ App (Idx 0) (Idx 1)) (Idx 2)

pprint a = putStrLn $ pShow a

main = do
  pprint $ eval "(Λ.Λ.(#0 #1) #2)"
  pprint $ eval "((Λ.Λ.(#0 #1) #2) x)"
  -- print $ fst $ fromString "((Λ.Λ.(#0 #1) #2) x)"
  pprint $ eval "(((Λ.Λ.Λ.(((#0 #1) #2) (x #2)) (#2 #3)) #2) #1)"
  -- print $ fst $ fromString "(((Λ.Λ.Λ.(((#0 #1) #2) (x #2)) (#2 #3)) #2) #1)"
