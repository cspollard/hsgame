{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Behavior where

import           Control.Arrow
import           Control.Category
import qualified Pipes            as P
import           Prelude          hiding (foldl, id, mapM, (.))

newtype BehaviorT m a b =
  BehaviorT { runBehaviorT :: a -> m (BehaviorT m a b, b) }
  deriving Functor

execBehaviorT :: Functor f => BehaviorT f a b -> a -> f (BehaviorT f a b)
execBehaviorT bh = fmap fst . runBehaviorT bh

evalBehaviorT :: Functor f => BehaviorT f a b -> a -> f b
evalBehaviorT bh = fmap snd . runBehaviorT bh

instance Applicative m => Applicative (BehaviorT m a) where
  pure x = BehaviorT $ \_ -> pure (pure x, x)
  BehaviorT x <*> BehaviorT y = BehaviorT $ \a ->
    (\(xab, xa) (yab, ya) -> (xab <*> yab, xa ya)) <$> x a <*> y a

instance Monad m => Category (BehaviorT m) where
  id = BehaviorT $ \a -> return (id, a)
  BehaviorT bc . BehaviorT ab = BehaviorT $ \a -> do
    (bhb, b) <- ab a
    (bhc, c) <- bc b
    return (bhc . bhb, c)

instance Monad m => Arrow (BehaviorT m) where
  arr f = let bh = BehaviorT $ \a -> return (bh, f a) in bh
  first (BehaviorT f) = BehaviorT $ \(a, c) -> do
    (bh, b) <- f a
    return (first bh, (b, c))

type Behavior a b = forall m. Monad m => BehaviorT m a b

cat :: Behavior a a
cat = id

map :: (a -> b) -> Behavior a b
map = arr

mapM :: Monad m => (a -> m b) -> BehaviorT m a b
mapM f = bh
  where
    bh = BehaviorT go
    go a = do
      b <- f a
      return (bh, b)

lift :: Monad m => (b -> m c) -> BehaviorT m a b -> BehaviorT m a c
lift f bh = mapM f . bh

foldlM :: Monad m => (b -> a -> m b) -> b -> BehaviorT m a b
foldlM f b = BehaviorT $ \a -> do
  b' <- f b a
  return (foldlM f b', b')

foldl :: (b -> a -> b) -> b -> Behavior a b
foldl f = foldlM (\b a  -> return $ f b a)


liftPipe :: Monad m => BehaviorT m a b -> P.Proxy () a () b m c
liftPipe bh = do
  a <- P.await
  (bh', b) <- P.lift $ runBehaviorT bh a
  P.yield b
  liftPipe bh'


unfoldrB :: Monad m => (t -> m ()) -> BehaviorT m t t -> t -> m ()
unfoldrB f bh a = do
  f a
  (bh', a') <- runBehaviorT bh a
  unfoldrB f bh' a'

mapMB :: Monad m => m a -> (b -> m c) -> BehaviorT m a b -> m ()
mapMB f g bh = do
  a <- f
  (bh', b) <- runBehaviorT bh a
  g b
  mapMB f g bh'

-- TODO
-- this throws away some information...
-- instance Monad m => Monad (BehaviorT a m) where
--   return = pure
--   BehaviorT f >>= g = BehaviorT $ \a -> do
--     (bh, b) <- f a
--     runBehaviorT (g b) a
