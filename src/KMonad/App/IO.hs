-- |

module KMonad.App.IO where

import KMonad.Prelude


import KMonad.App.Cmds
import KMonad.App.Configurable
import KMonad.App.Logging
import KMonad.App.Types

import Data.Monoid
import RIO.List (sort, headMaybe)
import UnliftIO.Directory

-- | The first runtime environment available to all of KMonad
data RootEnv = RootEnv
  { _geRootCfg   :: RootCfg
  , _geChanges   :: CfgChange -- ^ A record of the changes with which we were started
  , _geLogEnv    :: LogEnv
  , _geCmdsEnv   :: CmdsEnv
  }
makeClassy ''RootEnv


instance HasRootCfg RootEnv where rootCfg = geRootCfg
instance HasLogEnv  RootEnv where logEnv  = geLogEnv
instance HasCmdsEnv RootEnv where cmdsEnv = geCmdsEnv
instance HasTask    RootEnv where task    = bcTask

type CanRoot m env = ( EnvUIO m env, HasLogEnv env, HasRootEnv env
                     , HasRootCfg env, HasCmdsEnv env )

-- | Run a function that requires a 'RootEnv' using a 'RootCfg'
--
-- Note, this context handler will also catch any uncaught errors and print them
-- out using the initialized logging environment before exiting with failure.
withRoot :: (UIO m) => CfgChange -> (RootEnv -> m a) -> m a
withRoot chg f = let cfg = onDef chg in
  withLog cfg $ \logenv -> do
    withCmds logenv cfg $ \cmdenv -> do
      let go = do
            runRIO logenv $ do
              sep >> log "Applied following changes to default config"
              pp $ (chg^..folded.description)^.reversed
            f (RootEnv cfg chg logenv cmdenv)

      let oops err = do
            runRIO logenv . atError . log . ("Unrecoverable error: " <>) .
              pack . displayException $ (err :: SomeException)
            exitFailure

      go `catch` oops
