-- Valutatore con tracciamento delle operazioni

import Prelude hiding (return, (>>=))

data Expr = Const Int | Div Expr Expr
  deriving Show

-- Gestione esplicita del tracciamento
type Output a = (String, a)

eval :: Expr -> Output Int
eval (Const n) = (line (Const n) n, n)
eval (Div t s) =
  let (x, m) = eval t in
    let (y, n) = eval s in
      (x ++ y ++ line (Div t s) (m `div` n), m `div` n)

line :: Expr -> Int -> String
line t n = "eval (" ++ show t ++ ") = " ++ show n ++ "\n"

-- Definizione della monade Output
-- Versione con gestione monadica del tracciamento

return :: a -> Output a
return a = ("", a)

(>>=) :: Output a -> (a -> Output b) -> Output b
(>>=) m f =
  let (x, a) = m in
    let (y, b) = f a in
      (x ++ y, b)

output :: String -> Output ()
output x = (x, ())

evalM :: Expr -> Output Int
evalM (Const n) =
  output (line (Const n) n) >>= \() ->
  return n
evalM (Div t s) =
  evalM t >>= \m ->
  evalM s >>= \n ->
  output (line (Div t s) (m `div` n)) >>= \() -> return (m `div` n)

expr :: Expr
expr = Div (Div (Const 1972) (Const 2)) (Const 23)
