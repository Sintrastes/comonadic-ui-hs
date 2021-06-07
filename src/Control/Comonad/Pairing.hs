module Control.Comonad.Pairing where

import Control.Comonad

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> c) -> f a -> g b -> c
