
module Control.UIFramework.Gtk where

import GI.Gtk.Declarative
import Control.UIFramework

-- Note: In order for this to work, I would need
-- to define my own widget type. The one for
-- gi-gtk-declaraitive is only generic in its
-- event type.

data GtkComponent window m a = GtkComponent {
  view  :: a -> Bin window Widget (m ()),
  state :: a
}

instance UIFramework IO GtkComponent where
--  runComponent component = undefined


-- | Run an 'App'. This IO action will loop, so run it in a separate thread
-- using 'async' if you're calling it before the GTK main loop.
-- Note: the following example take care of exception raised in 'runLoop'.
--
-- @
--     void $ Gtk.init Nothing
--     main <- Async.async Gtk.main
--     runLoop app `finally` (Gtk.mainQuit >> Async.wait main)
-- @
runLoop :: (Comonad w, Pairing w m, Gtk.IsBin window) => GtkComponent window m a -> IO a
runLoop GtkComponent {..} = do
  let firstMarkup = view state

  ref                        <- newChan
  (firstSpace, subscription) <- do
    firstSpace <- runUI (create firstMarkup)
    runUI (Gtk.widgetShowAll =<< someStateWidget firstSpace)
    sub <- subscribe firstMarkup firstSpace (publishEvent ref)
    return (firstSpace, sub)

  Async.withAsync (runProducers ref inputs) $ \inputs' -> do
    Async.withAsync (wrappedLoop firstSpace firstMarkup ref subscription) $ \loop' -> do
      Async.waitEither inputs' loop' >>= \case
        Left _      -> Async.wait loop'
        Right state -> state <$ Async.uninterruptibleCancel inputs'

 where
  wrappedLoop firstSpace firstMarkup ref subscription =
    loop firstSpace firstMarkup ref subscription initialState
      -- Catch exception of linked thread and reraise them without the
      -- async wrapping.
      `catch` (\(Async.ExceptionInLinkedThread _ e) -> throwIO e)

  loop :: (Comonad w, Pairing w m, Gtk.IsBin window) => w a -> Bin window Widget (m ()) -> Chan (m ()) -> Subscription -> a -> IO a
  loop oldState oldMarkup ref oldSubscription oldModel = do
    space <- readChan ref
    case update oldModel event of
      Transition newModel action -> do
        let newMarkup = view newModel

        (newState, sub) <- case patch oldState oldMarkup newMarkup of
          Modify ma -> runUI $ do
            cancel oldSubscription
            newState <- ma
            sub      <- subscribe newMarkup newState (publishEvent ref)
            return (newState, sub)
          Replace createNew -> runUI $ do
            Gtk.widgetDestroy =<< someStateWidget oldState
            cancel oldSubscription
            newState <- createNew
            Gtk.widgetShowAll =<< someStateWidget newState
            sub <- subscribe newMarkup newState (publishEvent ref)
            return (newState, sub)
          Keep -> return (oldState, oldSubscription)

        -- If the action returned by the update function produced an event, then
        -- we write that to the channel.
        -- This is done in a thread to avoid blocking the event loop.
        a <- Async.async $
          -- TODO: Use prioritized queue for events returned by 'update', to take
          -- precendence over those from 'inputs'.
          action >>= maybe (return ()) (writeChan ref)

        -- If any exception happen in the action, it will be reraised here and
        -- catched in the thread. See the ExceptionInLinkedThread
        -- catch.
        Async.link a

        loop newState newMarkup ref sub newModel
    -- Exit -> return oldModel
    

-- | Assert that the program was linked using the @-threaded@ flag, to
-- enable the threaded runtime required by this module.
assertRuntimeSupportsBoundThreads :: IO ()
assertRuntimeSupportsBoundThreads = unless rtsSupportsBoundThreads $ do
  hPutStrLn
    stderr
    "GI.Gtk.Declarative.App.Simple requires the program to \
                     \be linked using the threaded runtime of GHC (-threaded \
                     \flag)."
  exitFailure

publishEvent :: Chan event -> event -> IO ()
publishEvent mvar = void . writeChan mvar

runProducers :: Chan event -> [Producer event IO ()] -> IO ()
runProducers chan producers =
  Async.forConcurrently_ producers $ \producer -> do
    runEffect $ producer >-> Pipes.mapM_ (publishEvent chan)
    performGC

runUI :: IO a -> IO a
runUI ma = do
  r <- newEmptyMVar
  runUI_ (ma >>= putMVar r)
  takeMVar r

runUI_ :: IO () -> IO ()
runUI_ ma = do
  tId <- myThreadId

  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    -- Any exception in the gtk ui thread will be rethrown in the calling thread.
    -- This ensure that this exception won't terminate the application without any control.
    ma `catch` throwTo @SomeException tId
    return False

