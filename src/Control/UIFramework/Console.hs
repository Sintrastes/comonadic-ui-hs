
module Control.UIFramework.Console where

import Control.UIFramework
import Control.Comonad
import Control.Comonad.Pairing
import Data.IORef
import Control.Monad

data Console a = Console { 
  state   :: a, 
  display :: a -> String,
  parse   :: String -> Maybe a,
  action  :: a -> IO () 
}

instance UIFramework IO Console where
  runComponent component = do 
    ref <- newIORef component   -- initialize the reference with the initial space
    forever $ do
      space <- readIORef ref
      -- send receives an action dispatched by the UI
      -- and updates the state by moving around in the space
      let send baseAction = do
            action <- baseAction
            writeIORef ref (move space action)
      -- extract the current interface.
      let Console state display parse action = extract space send
      -- Request input from the user.
      input <- getLine
      -- Run the action on the user's input.
      case parse input of
        Just value -> action value
        Nothing    -> return ()
