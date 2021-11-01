module KMonad.App.Main.Main ( main ) where



import KMonad.Prelude

import KMonad.App.Logging

import KMonad.App.Invocation    (getInvocation)
import KMonad.App.Main.Discover (runDiscover)
import KMonad.App.Main.Run      (runRun)
import KMonad.App.Operations
import KMonad.App.Types
import KMonad.App.Configurable

import System.Keyboard
import System.Keyboard.IO

-- | The entrypoint of KMonad
--
-- 1. Parse the command-line invocation
-- 2. Initialize the global context
-- 3. Dispatch on the Task
main :: OnlyIO ()
main = do
  invoc <- getInvocation
  let cfg = onDef invoc
  runRoot cfg $ do

    sep >> log "Welcome to KMonad"
    sep >> log "Invocation made the following changes:"
    pp $ invoc^.changes
    sep >> log "Starting KMonad with the following configuration:"
    pp cfg

    runTask

  -- let logcfg = (def :: LogCfg)
  --       & logLvl .~ (bascfg^.logLevel)
  --       -- & logLvl .~ LevelDebug
  --       & logSep .~ (if bascfg^.logSections then line else noSep)

  -- withLogging logcfg $ \logenv -> runRIO logenv $ do


    -- withKeyTable (bascfg^.keyTableCfg) $ \keytbl -> do

    --   let basenv = BasicEnv
    --             { _geRootCfg   = bascfg
    --             , _geLogEnv     = logenv
    --             , _geKeyTable   = keytbl
    --             }


-- | Run the task
runTask :: CanRoot m env => Task -> m ()
runTask = view task >>= \case
  (Discover cfg) -> runDiscover cfg
  ParseTest      -> atError $ log "parsetest!"
  (Run cfg)      -> runRun cfg
