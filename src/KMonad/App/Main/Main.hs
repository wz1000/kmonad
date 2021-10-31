module KMonad.App.Main.Main ( main ) where



import KMonad.Prelude

import KMonad.Util.Logging

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
  let bascfg = onDef invoc
  pp $ invoc^.changes
  runBasic bascfg $ do

    sepDebug
    logDebug "Starting KMonad with the following configuration:"
    logDebug . ppRecord $ bascfg

    runTask $ bascfg^.task

  -- let logcfg = (def :: LogCfg)
  --       & logLvl .~ (bascfg^.logLevel)
  --       -- & logLvl .~ LevelDebug
  --       & logSep .~ (if bascfg^.logSections then line else noSep)

  -- withLogging logcfg $ \logenv -> runRIO logenv $ do


    -- withKeyTable (bascfg^.keyTableCfg) $ \keytbl -> do

    --   let basenv = BasicEnv
    --             { _geBasicCfg   = bascfg
    --             , _geLogEnv     = logenv
    --             , _geKeyTable   = keytbl
    --             }


-- | Run the task
runTask :: CanBasic m env => Task -> m ()
runTask (Discover cfg) = runDiscover cfg
runTask ParseTest = logError "parsetest!"
runTask (Run cfg) = runRun cfg
