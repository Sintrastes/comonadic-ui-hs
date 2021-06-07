
module Control.Comonad.Moore where

import Control.Comonad
import Control.Comonad.Pairing

data Moore i a = Moore a (i -> Moore i a)

instance Functor (Moore i) where
  fmap f (Moore x rest) = Moore (f x) (\x -> fmap f (rest x))

instance Comonad (Moore i) where
  extract (Moore a _) = a
  duplicate w@(Moore a t) = Moore w (duplicate . t)
  
data Actions i a = NoAction a |Action (i, Actions i a) 

instance Pairing (Actions i) (Moore i) where
  pair f (NoAction a) (Moore b _)    = f a b
  pair f (Action (i, r)) (Moore _ t) = pair f r (t i)
  
unfoldMoore :: s -> (s -> (a, (i -> s))) -> Moore i a
unfoldMoore state next = Moore a (\input -> unfoldMoore (transition input) next)
  where (a, transition) = next state
