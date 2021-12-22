-- |

module Edifice.Types
  ( Cfg
  , CfgChange

  , module X
  )
where

import Util

import Art
import Edifice.Task

import Keyboard.Types      as X

{- SECTION --------------------------------------------------------------------}


type Cfg = (BaseCfg, Task)
type CfgChange = Changes Cfg

{- SECTION: RootCtx -----------------------------------------------------------}


-- -- | Configuration options that are required for everything in KMonad
-- data RootCfg = RootCfg
--   { _rLogCfg  :: LogCfg         -- ^ Logging cfg
--   , _rCmdsCfg :: CmdsCfg        -- ^ Hooks and commands cfg
--   , _cmdAllow :: Bool           -- ^ Whether to allow KMonad to call shell commands
--   , _cfgFile  :: Maybe FilePath -- ^ Where to look for a config file
--   } deriving (Eq, Show)
-- makeClassy ''RootCfg

-- -- | The first runtime environment available to all of KMonad
-- data RootEnv = RootEnv
--   {
--   --   _geRootCfg   :: RootCfg
--   -- , _geChanges   :: CfgChange -- ^ A record of the changes with which we were started
--   -- , _geLogEnv    :: LogEnv
--   -- , _geCmdsEnv   :: CmdsEnv
--   }
-- makeClassy ''RootEnv


-- -- instance HasRootCfg RootEnv where rootCfg = geRootCfg
-- -- instance HasLogEnv  RootEnv where logEnv  = geLogEnv
-- -- instance HasCmdsEnv RootEnv where cmdsEnv = geCmdsEnv
-- -- instance HasTask    RootEnv where task    = bcTask

-- -- type CanRoot m env = ( EnvUIO m env, HasLogEnv env, HasRootEnv env
-- --                      , HasRootCfg env, HasCmdsEnv env )

-- type CanRoot m env = ( EnvUIO m env ) --, HasLogEnv env, HasRootEnv env
--                      -- , HasRootCfg env, HasCmdsEnv env )

-- withRoot :: (UIO m) => a -> (RootEnv -> m a) -> m a
-- withRoot = undefined

-- | Run a function that requires a 'RootEnv' using a 'RootCfg'
--
-- Note, this context handler will also catch any uncaught errors and print them
-- out using the initialized logging environment before exiting with failure.
-- withRoot :: (UIO m) => CfgChange -> (RootEnv -> m a) -> m a
-- withRoot chg f = let cfg = onDef chg in
--   withLog cfg $ \logenv -> do
--     withCmds logenv cfg $ \cmdenv -> do
--       let go = do
--             runRIO logenv $ do
--               sep >> log "Applied following changes to default config"
--               pp $ (chg^..folded.description)^.reversed
--             f (RootEnv cfg chg logenv cmdenv)

--       let oops err = do
--             runRIO logenv . atError . log . ("Unrecoverable error: " <>) .
--               pack . displayException $ (err :: SomeException)
--             exitFailure

--       go `catch` oops
