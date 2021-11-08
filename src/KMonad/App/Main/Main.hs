module KMonad.App.Main.Main ( main ) where



import Preface

import KMonad.App.Logging

import KMonad.App.Cmds
import KMonad.App.IO
import KMonad.App.Invocation     (getInvocation)
import KMonad.App.Task.Discover  (runDiscover)
import KMonad.App.Task.ParseTest (runParseTest)
import KMonad.App.Task.Run       (runRun)
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
  chg <- getInvocation
  withRoot chg . inEnv $ do
    sep >> log "Starting KMonad with the following configuration:"
    pp =<< view rootCfg

    bracket_ (triggerHook OnStart) (triggerHook OnExit) runTask

-- | Run the task
runTask :: CanRoot m env => m ()
runTask = view (rootEnv.task) >>= \case
  (Discover _) -> runDiscover
  ParseTest    -> runParseTest
  (Run _)      -> runRun
