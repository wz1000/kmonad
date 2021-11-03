{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module KMonad.App.Logging where

import KMonad.Prelude

import Text.Pretty.Simple (pShow, pShowNoColor, pShowLightBg)
import RIO.Text.Lazy (toStrict)
import qualified RIO.Text as T
import qualified RIO as R

{- SECTION: Types -------------------------------------------------------------}

-- | The different ways in which we color some of our logging output
data LogColor
  = LightBG    -- ^ Colors chosen with a light background in mind
  | DarkBG     -- ^ Colors chosen with a dark background in mind
  | Monochrome -- ^ No colors
  deriving (Eq, Show)

instance Default LogColor where def = DarkBG

-- | The full logging configuration for KMonad
data LogCfg = LogCfg
  { _logLevel  :: !LogLevel -- ^ What level of log messages to display
  , _logColor  :: !LogColor -- ^ Whether to use color for pretty-print
  , _useSep    :: !Bool     -- ^ Whether to use log separators
  , _logTarget :: !Handle   -- ^ Where to log to
  } deriving (Eq, Show)
makeClassy ''LogCfg

instance Default LogCfg where def = LogCfg LevelWarn def True stdout

-- | The runtime environment for logging
data LogEnv = LogEnv
  { _leLogCfg :: !LogCfg   -- ^ Copy of the config
  , _logAt    :: !LogLevel -- ^ Current level of logging
  , _logFunc  :: !LogFunc  -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasLogCfg  LogEnv where logCfg   = leLogCfg
instance HasLogFunc LogEnv where logFuncL = logFunc

type CanLog m env = (MonadReader env m, HasLogEnv env, MonadIO m)

{- SECTION: IO ----------------------------------------------------------------}

-- | Use 'LogCfg' to run a function in the context of an acquired logger
withLog :: (UIO m, HasLogCfg c) => c -> (LogEnv -> m a) -> m a
withLog c f = do
  -- Default to non-verbose logging
  raw <- logOptionsHandle (c^.logTarget) False
  withLogFunc (raw & setLogMinLevel (c^.logLevel)) $
    f . LogEnv (c^.logCfg) LevelDebug

-- | Use 'LogCfg' to run a basic 'RIO LogEnv' monad
runLog :: HasLogCfg c => c -> (RIO LogEnv a) -> OnlyIO a
runLog c = withLog c . inEnv


-- | Express some text at the current level
log :: CanLog m env => Text -> m ()
log t = view logAt >>= \case
  LevelDebug -> liftLog R.logDebug t
  LevelInfo  -> liftLog R.logInfo  t
  LevelWarn  -> liftLog R.logWarn  t
  LevelError -> liftLog R.logError t
  _          -> nil
  where liftLog f t = view logFunc >>= \lf -> runRIO lf . f . display $ t

-- | Pretty-print a haskell-value
pp :: (CanLog m env, Show a) => a -> m ()
pp a = do
  f <- view (logEnv.logColor) >>= \case
    DarkBG     -> pure pShow
    LightBG    -> pure pShowLightBg
    Monochrome -> pure pShowNoColor
  log . toStrict . f $ a

-- | Print a value by its display instance
dsp :: (CanLog m env, Display a) => a -> m ()
dsp = log . textDisplay

-- | Print out the logging separator
sep :: CanLog m env => m ()
sep = view (logEnv.useSep) >>= \b -> when b (log $ "\n" <> T.replicate 80 "-")

-- | Set the current log level to some value for a statement
--
-- Note the difference between RIO logging and KMonad logging. In RIO logging
-- looks like this:
-- logDebug $ some statement
-- logDebug $ some other statement
--
-- In KMonad, logging looks like this:
-- atDebug $ do
--   log $ some statement
--   log $ some other statement
--
-- The default logging 'at-level' is LogDebug
atDebug, atInfo, atWarn, atError :: CanLog m env => m a -> m a
atDebug = locally logAt (const LevelDebug)
atInfo  = locally logAt (const LevelInfo)
atWarn  = locally logAt (const LevelWarn)
atError = locally logAt (const LevelError)
