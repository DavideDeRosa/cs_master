-- Valutatore con gestione della divisione per zero

import Prelude hiding (return, (>>=))

data Expr = Const Int | Div Expr Expr

-- Versione con gestione esplicita della divisione per zero
eval :: Expr -> Maybe Int
eval (Const n) = Just n
eval (Div t s) =
  case eval t of
    Nothing -> Nothing
    Just m  -> case eval s of
                 Nothing -> Nothing
                 Just 0  -> Nothing
                 Just n  -> Just (m `div` n)

-- Definizione della monade Maybe
-- Versione con gestione monadica della divisione per zero

return :: a -> Maybe a
return = Just

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) Nothing  _ = Nothing
(>>=) (Just a) f = f a

abort :: Maybe a
abort = Nothing

evalM :: Expr -> Maybe Int
evalM (Const n) = return n
evalM (Div t s) =
  evalM t >>= \m ->
  evalM s >>= \n ->
  if n == 0 then abort
  else return (m `div` n)

expr :: Expr
expr = Div (Const 1492) (Const 27)

main :: IO ()
main = print (evalM expr)
