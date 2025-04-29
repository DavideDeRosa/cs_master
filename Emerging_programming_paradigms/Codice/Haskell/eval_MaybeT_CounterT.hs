-- Valutatore in cui MaybeT e CounterT sono monad transformers (e si definisce
-- la monade identità "per partire")

import Prelude hiding (Monad, return, (>>=))

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

class MonadT t where
    lift :: Monad m => m a -> t m a

-- Definizione della monade Id

newtype Id a = Id { unId :: a }

instance Monad Id where
    return = Id . id
    (>>=) m f = f (unId m)

-- Definizione del monad transformer CounterT

newtype CounterT m a = CounterT { unCounterT :: Int -> m (a, Int) }

instance Monad m => Monad (CounterT m) where
    return a = CounterT $ \x -> return (a, x)
    (>>=) m f = CounterT $ \x ->
        unCounterT m x >>= \(a, y) -> unCounterT (f a) y

tick :: Monad m => CounterT m ()
tick = CounterT $ \x -> return ((), x + 1)

instance MonadT CounterT where
    lift m = CounterT $ \x -> m >>= \a -> return (a, x + 1)

-- Definizione del monad transformer MaybeT

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

instance MonadT MaybeT where
    lift m = MaybeT $ m >>= \a -> return (Just a)

-- main

data Expr = Const Int | Div Expr Expr

type MaybeCounter = MaybeT (CounterT Id)

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
main = print (unId (unCounterT (unMaybeT (evalM expr)) 0))
