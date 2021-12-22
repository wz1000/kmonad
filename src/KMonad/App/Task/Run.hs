module KMonad.App.Task.Run

where

import Preface
import KMonad.App.Locale
import KMonad.App.Logging
import KMonad.App.IO
import KMonad.App.Types

import Keyboard
-- import System.Keyboard.IO

{- SECTION: types -------------------------------------------------------------}

data RunEnv = RunEnv
  { _reRunCfg    :: RunCfg
  , _reRootEnv   :: RootEnv
  , _reLocaleEnv :: LocaleEnv
  , _reInputEnv      :: InputEnv
  , _reKeyO      :: KeyO
  }
makeClassy ''RunEnv

instance HasRunCfg    RunEnv where runCfg    = reRunCfg
instance HasRootEnv   RunEnv where rootEnv   = reRootEnv
instance HasLocaleEnv RunEnv where localeEnv = reLocaleEnv
instance HasLogEnv    RunEnv where logEnv    = rootEnv.logEnv
instance HasInputEnv      RunEnv where keyI      = reInputEnv
instance HasKeyO      RunEnv where keyO      = reKeyO



runRun :: CanRoot m env => RunCfg -> m ()
runRun _ = atError $ log "Run!"



-- import KMonad.App.KeyIO
-- import KMonad.App.Types
-- import KMonad.App.Parser.IO -- FIXME: change import when invoc/parse separation is clean
-- import KMonad.App.Main.Loop
-- import KMonad.App.Main.OS
-- import KMonad.Model hiding (withModel) -- FIXME: change when pullchain is factored out
-- import KMonad.Util hiding (logLvl)
-- import KMonad.Pullchain.IO

-- import KMonad.App.Main.Discover

-- import qualified RIO.Text.Lazy as T

-- import Text.Pretty.Simple

--------------------------------------------------------------------------------
-- $init

-- | The outermost error handler, pretty-print the exception and exit.
-- withHandler :: LUIO m e => Ctx r m ()
-- withHandler = mkCtx $ handle h . ($ ())
--   where h e = do
--           logError . pack . displayException $ (e :: SomeException)
--           liftIO exitFailure

-- -- | Initialize all the components of the KMonad app-loop
-- withAppEnv :: LUIO m e => AppCfg -> Ctx r m AppEnv
-- withAppEnv cfg = do


--   -- Acquire the keysource and keysink
--   src <- withKeyInput  $ cfg^.keyInputCfg
--   snk <- withKeyOutput $ cfg^.keyOutputCfg

--   -- Initialize the model with the model config
--   api <- withModel $ cfg^.pullchainCfg

--   let init = do
--         logDebug $ "Starting KMonad with following Cfg:\n" <> ppRecord cfg

--         lge <- view logEnv

--         -- Wait a bit for the user to release the 'Return' key with which they started
--         -- KMonad. If we don't do this, we run the risk of capturing the keyboard used
--         -- to start KMonad, resulting in a 'stuck' button.
--         wait $ cfg^.startDelay

--         pure $ AppEnv
--           { _keAppCfg   = cfg
--           , _keLogEnv   = lge
--           , _keySink    = snk
--           , _keySource  = src
--           , _aeModelAPI = api
--           }

--   let cleanup _ = logInfo "Exiting KMonad"

--   mkCtx $ bracket init cleanup



-- | Run KMonad using the provided configuration
-- startApp :: AppCfg -> OnlyLIO ()
-- startApp cfg = runCtx (withHandler >> withOS >> withAppEnv cfg) $ inEnv loop

-- | Execute the provided 'Cmd'
--
-- Wrapped in the context of OS-specific tweaks:
-- 1. Construct the log-func
-- 2. Parse the config-file
-- 3. Maybe start KMonad
--
-- TODO: This should dispatch more clearly on different tasks, tasks to add:
-- Key-Input mode: just print out key-events until interrupted.
-- Dry-Run mode: change dry-run mode from a flag to a command.
--
-- run :: Invoc -> OnlyIO ()
-- run c = do
--   -- FIXME: Make this actually do something instead of constructing a default log-cfg
--   let logcfg = LogCfg (c^.logLvl) stdout Nothing

--   runLog logcfg $ do
--     if c^.discMode
--       then runDiscover
--       else  do
--         cfg <- loadConfig (c^.cfgFile) c -- Load cfg-file and overwrite Invoc settings
--         unless (c^.dryRun) $ startApp cfg

-- newtype Socket m a = Socket { _uSocket :: ( a -> m (), m a ) }
-- type KeySocket m = Socket m KeySwitch

-- data KioCfg = LinCfg | WinCfg | MacCfg
