module KMonad.App.Main.Run

where

import KMonad.Prelude
import KMonad.App.Types
import KMonad.Util.Logging

runRun :: CanBasic m env => RunCfg -> m ()
runRun _ = logError "Run!"

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
