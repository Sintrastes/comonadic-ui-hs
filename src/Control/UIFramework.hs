
module Control.UIFramework where

import Control.Comonad
import Control.Comonad.Pairing

-- | Type of a declarative UI description with base monad b,
--   monad m to describe the UI actions, and a to describe 
--   the state of the UI.
--
--   Note: I'm not sure if this type allows for 
--   "input" and "output" types in a way
--   that facilitates combining input and output types
--   of components.
--
type UI b m f a = (b (m ()) -> b ()) -> f a

-- | Type of a declarative UI component with base monad b,
--   a pairing between a monad m and a comonad w to describe 
--   the UI actions, and a to describe the state of the UI.
type Component b w m f a = w (UI b m f a)

move :: (Comonad w, Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ newSpace -> newSpace) movement (duplicate space)

class UIFramework b c where
  runComponent :: (Comonad w, Pairing m w) => Component b w m c a -> b a
