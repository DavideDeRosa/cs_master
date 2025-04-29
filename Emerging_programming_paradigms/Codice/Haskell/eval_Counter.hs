-- Valutatore con conteggio delle divisioni

import Prelude hiding (return, (>>=), (>>))

data Expr = Const Int | Div Expr Expr

-- Versione con gestione esplicita del contatore
type Counter a = Int -> (a, Int)

eval :: Expr -> Counter Int
eval (Const n) x = (n, x)
eval (Div t s) x =
  let (m, y) = eval t x in
    let (n, z) = eval s y in
      (m `div` n, z + 1)

-- Definizione della monade Counter
-- Versione con gestione monadica del contatore

return :: a -> Counter a
return a = \x -> (a, x)

(>>=) :: Counter a -> (a -> Counter b) -> Counter b
(>>=) m f = \x -> let (a, y) = m x in f a y

tick :: Counter ()
tick = \x -> ((), x + 1)

evalM :: Expr -> Counter Int
evalM (Const n) = return n
evalM (Div t s) =
  evalM t >>= \m ->
  evalM s >>= \n ->
  tick >>= \() ->
  return (m `div` n)

-- Versione con gestione monadica del contatore e >>
(>>) :: Counter a -> Counter b -> Counter b
(>>) as bs = as >>= const bs

evalN :: Expr -> Counter Int
evalN (Const n) = return n
evalN (Div t s) =
  evalN t >>= \m ->
  evalN s >>= \n ->
  tick >>
  return (m `div` n)

expr :: Expr
expr = Div (Const 1492) (Const 27)

main :: IO ()
main = print (evalM expr 0)
