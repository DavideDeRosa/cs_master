-- Parser monadico

import Prelude hiding (return, (>>=), (>>))
import Data.Char

data Expr = Const Int | Div Expr Expr
  deriving Show

-- Definizione della monade Parser

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return a = \s -> [(a, s)]

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(>>=) p f = \s -> [ (b, v) | (a, u) <- p s, (b, v) <- f a u ]

(>>) :: Parser a -> Parser b -> Parser b
(>>) p q = p >>= const q

item :: Parser Char
item [] = []
item (c : cs) = [(c, cs)]

none :: Parser a
none _ = []

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q s = p s ++ q s

(|>) :: Parser a -> (a -> Bool) -> Parser a
(|>) p f s = [ (a, t) | (a, t) <- p s, f a ]

plus :: Parser a -> Parser [a]
plus p = p >>= \a -> star p >>= \as -> return (a : as)

star :: Parser a -> Parser [a]
star p = plus p <|> return []

digit :: Parser Char
digit = item |> isDigit

digits :: Parser String
digits = star digit

digitp :: Parser String
digitp = plus digit

int :: Parser Int
int = digitp >>= \ds -> return (read ds)

lit :: Char -> Parser Char
lit c = item |> (== c)

-- Parser per espressioni, partiamo dalla seguente grammatica

-- T ::= F | T / F
-- F ::= n | (T)

-- Eliminiamo dalla grammatica la ricorsione a sinistra

-- T  ::= FT'
-- T' ::= eps | /FT'
-- F ::= n | (T)

term :: Parser Expr
term = factor >>= \f -> term1 >>= \fs -> return (foldl Div f fs)

term1 :: Parser [Expr]
term1 = star (lit '/' >> factor)

factor :: Parser Expr
factor = (int >>= (return . Const)) <|>
         (lit '(' >> term >>= \t -> lit ')' >> return t)

main :: IO ()
main = print (term "1492/55")
