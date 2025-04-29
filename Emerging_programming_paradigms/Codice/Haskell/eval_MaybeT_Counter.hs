-- Valutatore in cui MaybeT è una monad transformer e Counter è la monade
-- definita in precedenza

import Prelude hiding (Monad, return, (>>=))

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

-- Definizione della monade Counter

newtype Counter a = Counter { unCounter :: Int -> (a, Int) }

instance Monad Counter where
    return a = Counter $ \x -> (a, x)
    (>>=) m f = Counter $ \x -> let (a, y) = unCounter m x in unCounter (f a) y

tick :: Counter ()
tick = Counter $ \x -> ((), x + 1)

-- Definizione del monad transformer MaybeT

class MonadT t where
    lift :: Monad m => m a -> t m a

instance MonadT MaybeT where
    lift m = MaybeT $ m >>= (return . Just)

newtype MaybeT m a = MaybeT { unMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    (>>=) m f = MaybeT $
        unMaybeT m >>= \x ->
            case x of
                Nothing -> return Nothing
                Just a  -> unMaybeT (f a)

abort :: Monad m => MaybeT m a
abort = MaybeT (return Nothing)

data Expr = Const Int | Div Expr Expr

type MaybeCounter = MaybeT Counter

evalM :: Expr -> MaybeCounter Int
evalM (Const n) = return n
evalM (Div t s) =
    evalM t >>= \m ->
    evalM s >>= \n ->
    lift tick >>= \() ->
    if n == 0 then abort
    else return (m `div` n)

expr :: Expr
expr = Div (Const 2) (Const 0)

main :: IO ()
main = print (unCounter (unMaybeT (evalM expr)) 0)
