-- Valutatore con gestione della divisione per zero e sintassi do

data Expr = Const Int | Div Expr Expr

abort :: Maybe a
abort = Nothing

evalM :: Expr -> Maybe Int
evalM (Const n) = return n
evalM (Div t s) = do
    m <- evalM t
    n <- evalM s
    if n == 0 then abort
    else return (m `div` n)

expr :: Expr
expr = Div (Const 1492) (Const 27)

main :: IO ()
main = print (evalM expr)
