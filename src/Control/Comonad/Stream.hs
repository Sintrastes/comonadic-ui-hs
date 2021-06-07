
module Control.Comonad.Stream where

import Control.Comonad.Pairing
import Control.Comonad

data Sequence a = End a | Next (Sequence a) 

instance Functor Sequence where
  fmap f (End a)  = End $ f a
  fmap f (Next x) = fmap f x

instance Applicative Sequence where
  pure = return
  f <*> x = f >>= (\f' -> 
    x >>= (\x' ->
      return $ f' x'
    )
   )

instance Monad Sequence where
  return = End
  (End a)     >>= f = f a 
  (Next next) >>= f = Next (next >>= f)

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate stream@(Cons a as) = Cons stream (duplicate as)
  
instance Pairing Sequence Stream where
  pair f (End a) (Cons b _) = f a b
  pair f (Next next) (Cons _ stream) = pair f next stream

unfoldStream :: s -> (s -> (a, s)) -> Stream a
unfoldStream initialState next = Cons a (unfoldStream nextState next)
  where (a, nextState) = next initialState
