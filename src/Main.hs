module Main where

import Control.Monad
import Control.Comonad
import Control.Comonad.Pairing
import Control.Comonad.Stream
import Control.UIFramework
import Control.UIFramework.Console

counter :: Component IO Stream Sequence Console Int
counter = unfoldStream 0 (\state -> (render state, state + 1))
  where
    render :: Int -> UI IO Sequence Console Int
    render state = \send -> 
       Console {
         state   = state,        -- display the current counter value
         display = show,
         parse = return . read,
         action = \input -> send $ do
           print state
           return $ Next $ End () -- move to next state when of user input
       }
    
-- Here's an example of how I might have an applicative form
-- interface based on this: (except using reification)

-- instance Applicative (Component IO Stream Sequence Widget) where
--   pure x  = undefined -- empty component returning x
--   f <*> x = undefined 

main :: IO Int
main = runComponent counter
