-- Valutatore senza effetti

data Expr = Const Int | Div Expr Expr

eval :: Expr -> Int
eval (Const n) = n
eval (Div t s) = eval t `div` eval s

expr :: Expr
expr = Div (Const 1492) (Const 27)

main :: IO ()
main = print (eval expr)
