-- |

module KMonad.App.CfgFile.IO where

import KMonad.Prelude

import KMonad.App.Configurable
import KMonad.App.Logging
import KMonad.App.Types
import KMonad.App.CfgFile.Tokenizer
import KMonad.App.CfgFile.Grokker
import KMonad.Util.Ctx

-- import KMonad.Model.Types
import KMonad.App.IO


import Data.Monoid
import UnliftIO.Directory
import RIO.List (partition, sort, headMaybe)

import qualified RIO.Text as T



type CfgFile = FilePath

-- | Load, tokenize, and grok a configuration from file.
--
-- Note that this can throw standard IO errors (file not found, etc.), tokenize
-- errors for basic syntax errors, and grok errors, for invalid configuration
-- structure (like double defsrc blocks) and such.
loadCfgFile :: IO m => CfgFile -> m CfgChange
loadCfgFile pth = do
  txt <- readFileUtf8 pth
  tks <- either throwIO pure (tokenize txt)
  either throwIO pure (grok tks)

-- | Return the configuration file to load
--
-- 1. Check the passed config
-- 2. Check $XDG_CONFIG/kmonad/default.kbd
-- 3. Alphabetically-first: $XDG_CONFIG/kmonad/*.kbd
-- 4. Nothing
getCfgFile :: (CanRoot m env) => m (Maybe CfgFile)
getCfgFile = do
  cfg <- view cfgFile
  xdg <- getXdgDirectory XdgConfig "kmonad"
  ifM (doesPathExist xdg) (pure cfg) $ do
    xfs <- listDirectory xdg
    pure . getAlt . mconcat . map Alt $
      [ cfg
      , fmap (xdg </>) $ xfs^?folded.filtered (== "default.kbd")
      , fmap (xdg </>) $ headMaybe . sort $ xfs
      ]

-- | Load a configuration from file, apply its changes to the root env, and run
-- the provided action in the updated environment.
--
-- Note that we are already in a 'RootEnv' when this is called, so we get the
-- config-file directly from the env. Then we load all changes to the 'RootCfg'
-- from file, reinitialize a 'RootEnv', and run the provided action inside this
-- new env.
withLoadedCfg :: CanRoot m env => m a -> m a
withLoadedCfg a = getCfgFile >>= \case
  Nothing  -> (atInfo $ sep >> log "Not loading configuration") >> a
  Just pth -> do
    sep >> (atInfo . log $ "Loading configuration from: " <> pack pth)
    new <- loadCfgFile pth
    (tsk, rst) <- partition (\a -> T.isPrefixOf "task" $ a^.description) <$> view geChanges

    withRoot (rst <> new <> tsk) $ \newenv ->
      locally rootEnv (const newenv) $ do
        sep >> log "Continuing with the following configuration:"
        pp =<< view rootCfg
        a

ctxLoadedCfg :: CanRoot m env => Ctx r m ()
ctxLoadedCfg = mkCtx $ \f -> withLoadedCfg $ f ()
