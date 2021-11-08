module KMonad.Util.Logging.Types
  ( LogCfg(..)
  , LogEnv(..)
  , LogLevel(..)

  , LogSep(..)
  , line
  , noSep

  , HasLogCfg(..)
  , HasLogEnv(..)

  , LIO, LUIO, OnlyLIO
  )
where


import Preface
import RIO (LogLevel(..))
import qualified RIO.Text as T


--------------------------------------------------------------------------------
-- $cfg

newtype LogSep = LogSep { _uLogSep :: Maybe Text }
  deriving (Eq, Show)

line, noSep :: LogSep
line  = LogSep . Just $ "\n" <> T.replicate 80 "-"
noSep = LogSep Nothing

instance Default LogSep where def = line

-- | The full logging configuration for KMonad
data LogCfg = LogCfg
  { _logLvl :: !LogLevel -- ^ What level to log at
  , _logTgt :: !Handle   -- ^ Where to log to
  , _logSep :: !LogSep   -- ^ If and what to use as section separator
  } deriving Show
makeClassy ''LogCfg

instance Default LogCfg where
  def = LogCfg
    { _logLvl = LevelWarn
    , _logTgt = stdout
    , _logSep = def
    }


--------------------------------------------------------------------------------
-- $env

-- | The logging runtime environment
data LogEnv = LogEnv
  { _leLogCfg  :: !LogCfg -- ^ Copy of the config
  , _logFunc :: !LogFunc  -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasLogCfg  LogEnv where logCfg   = leLogCfg
instance HasLogFunc LogEnv where logFuncL = logFunc


--------------------------------------------------------------------------------
-- $shorthand

-- | Type shorthand for ReaderT-IO-with-logging
type LIO m env  = (EnvIO m env, HasLogEnv env)
type LUIO m env = (LIO m env, UIO m)
type OnlyLIO a  = RIO LogEnv a
