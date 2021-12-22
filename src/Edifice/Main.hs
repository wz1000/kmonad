-- |

module Edifice.Main where

import Util

import Art
import Edifice.Ctx
import Edifice.Invoc

-- | Setup the universal runtime environment for KMonad
runDais :: D a -> OnlyIO a
runDais d = do
  invoc <- getInvoc
  withLog (getLogCfg invoc) $ \logenv -> do

    -- The provided action with some additional logging on invoc
    let go = do
          runRIO logenv $ do
            sepLog "Applied following changes to default config"
            pp $ invoc^.description
          runRIO (Dais invoc logenv) d

    -- Handler that prints any exception and then exits with failure
    let oops err = do
          runRIO logenv . atError . sepLog . ("Unrecoverable error: " <>) .
            pack . displayException $ (err :: SomeException)
          exitFailure

    go `catch` oops

-- | The entrypoint of KMonad
--
-- 1. Parse the command-line invocation
-- 2. Initialize the global context
-- 3. Dispatch on the Task
main :: OnlyIO ()
main = runDais do
  sepLog "Welcome to KMonad, the friendly onion of keyboard management."
  view (invoc.task) >>= \case
    Discover  -> runDiscover
    ParseTest -> runParseTest
    Run       -> runRun
