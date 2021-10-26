-- FIXME: Move the CPP somewhere else
{-# LANGUAGE CPP #-}
module KMonad.App.Types
  -- ( AppCfg(..)
  -- , AppEnv(..)
  -- , HasAppCfg(..)
  -- , HasAppEnv(..)
  -- , App
  -- )
where

import KMonad.Prelude

-- import KMonad.App.KeyIO
-- import KMonad.Model.Types
import KMonad.Util

import System.Keyboard

import UnliftIO.Directory
import qualified RIO.Text as T
import RIO.FilePath

--------------------------------------------------------------------------------
-- $appcfg
--
-- The 'AppCfg' and 'AppEnv' records store the configuration and runtime
-- environment of KMonad's app-loop respectively. This contains nearly all of
-- the components required to run KMonad.
--
-- Note that the 'AppEnv' is still not sufficient to satisfy 'MonadK', since
-- there are times where we are not processing a button push. I.e. 'MonadK' is a
-- series of operations that allow us to specify how to deal with the current
-- button-push, but it required us to have actually registered a push (or
-- release) of some button. 'AppEnv' exists before any buttons have been pushed,
-- and therefore contains no information about 'the current button push' (nor
-- could it). Later in this module we specify KEnv as a combination of AppEnv
-- and a BEnv. It is that environment that we use to satisfy 'MonadK'.

-- | Record of all the configuration options required to run KMonad's core App
-- loop.
-- data AppCfg = AppCfg
--   { _keyInputCfg  :: KeyInputCfg  -- ^ The configuration of the input keyboard
--   , _keyOutputCfg :: KeyOutputCfg -- ^ The configuration of the output keyboard
--   , _acModelCfg   :: ModelCfg     -- ^ The keymap/model configuration
  -- , _allowCmd     :: Bool         -- ^ Whether shell-commands are allowed
  -- , _startDelay   :: Ms           -- ^ How long to wait before acquiring the input keyboard
--   } deriving Show
-- makeClassy ''AppCfg

-- instance HasModelCfg AppCfg where modelCfg = acModelCfg

-- -- | Environment of a running KMonad app-loop
-- data AppEnv = AppEnv
--   { -- Stored copy of cfg
--     _keAppCfg   :: AppCfg

--     -- General IO
--   , _keLogEnv   :: LogEnv
--   , _keySource  :: GetKey
--   , _keySink    :: PutKey

--     -- API to the model
--   , _aeModelAPI :: ModelAPI
--   }
-- makeClassy ''AppEnv

-- instance HasLogEnv AppEnv where logEnv = keLogEnv
-- instance HasLogCfg AppEnv where logCfg = logEnv.logCfg
-- instance HasAppCfg AppEnv where appCfg = keAppCfg
-- instance HasModelAPI AppEnv where modelAPI = aeModelAPI

-- type App a = RIO AppEnv a


--------------------------------------------------------------------------------

{- SECTION: Functionality-specific configurations --------------------------------

There are a number of 'functionalities' that KMonad supports. Each of these can
be initialized independently of the other. We only ever initialize the
functionalities required for the specific task we've been instructed to carry
out.

required for: r -> run, d -> discover, p -> parsetest

| functionality | required for | description                             |
|---------------+--------------+-----------------------------------------|
| model         | r            | run a remapping model on keyboard input |
| input         | rd           | capture keyboard input from OS          |
| output        | r            | send keyboard output to OS              |
| basic         | rdp          | logging, keytable                       |

NOTE: we import input and output configuration types from System.Keyboard.
-------------------------------------------------------------------------------}

{- SUBSECTION: model ----------------------------------------------------------}

-- | Config options pertinent to running a KMonad model
data ModelCfg = ModelCfg
  { _fallthrough :: Bool    -- ^ Whether to rethrow uncaught key events
  , _composeKey  :: Keyname -- ^ What button to use as a compose-key
  , _macroDelay  :: Ms      -- ^ How long to pause between macro-taps
  } deriving (Eq, Show)
makeClassy ''ModelCfg

instance Default ModelCfg where def = ModelCfg True "ralt" 10

{- SECTION: task configs ------------------------------------------------------}


-- | Config describing how to run @kmonad run@
--
-- To do this we need to:
-- 1. Parse a config file
-- 2. Grab both KeyI and KeyO
-- 3. Run the model
data RunCfg = RunCfg
  { _rModelCfg  :: ModelCfg  -- ^ Cfg how the model is run
  , _rInputCfg  :: InputCfg  -- ^ Cfg how to grab input
  , _rOutputCfg :: OutputCfg -- ^ Cfg how to generate output
  } deriving (Eq, Show)
makeClassy ''RunCfg

instance Default RunCfg where def = RunCfg def def def

instance HasModelCfg  RunCfg where modelCfg  = rModelCfg
instance HasInputCfg  RunCfg where inputCfg  = rInputCfg
instance HasOutputCfg RunCfg where outputCfg = rOutputCfg

-- | Config describing how to run @kmonad discover@
--
-- To do this we need to:
-- 1. Grab KeyI
-- 2. Maybe parse a config file (to get at KeyI)
data DiscoverCfg = DiscoverCfg
  { _dInputCfg    :: InputCfg  -- ^ Config how to grab input
  , _dumpKeyTable :: Bool      -- ^ Flag indicating whether to dump table
  , _escapeExits  :: Bool      -- ^ Flag indicating whether to exit on escape
  } deriving (Eq, Show)
makeClassy ''DiscoverCfg

instance Default     DiscoverCfg where def      = DiscoverCfg def False True
instance HasInputCfg DiscoverCfg where inputCfg = dInputCfg

{- SECTION: task --------------------------------------------------------------}

-- | The instruction being passed to KMonad
data Task
  = Run RunCfg           -- ^ Run KMonad as a keyboard remapper
  | Discover DiscoverCfg -- ^ Print out information about an input device
  | ParseTest            -- ^ Test if a configuration file parser without error
  deriving (Eq, Show)
makeClassyPrisms ''Task

class HasTask a where task :: Lens' a Task
instance HasTask Task where task = id

-- | Configuration options that are required for everything in KMonad
data BasicCfg = BasicCfg
  { _logLevel    :: LogLevel       -- ^ Logging level
  , _logSections :: Bool           -- ^ Used to enable section-breaks in logging
  , _keyTableCfg :: KeyTableCfg    -- ^ Table of name-keycode correspondences
  , _cmdAllow    :: Bool           -- ^ Whether to allow KMonad to call shell commands
  , _cfgFile     :: Maybe FilePath -- ^ Where to look for a config file
  , _bcTask      :: Task           -- ^ The instruction passed to KMonad
  } deriving (Eq, Show)
makeClassy ''BasicCfg

instance Default BasicCfg where
  def = BasicCfg
    { _logLevel    = LevelWarn
    , _logSections = True
    , _keyTableCfg = EnUS
    , _cmdAllow    = False
    , _cfgFile     = Nothing
    , _bcTask      = Run def
    }

instance HasTask BasicCfg where task = bcTask

{- NOTE: global environment ---------------------------------------------------}

-- | The first runtime environment available to all of KMonad
data BasicEnv = BasicEnv
  { _geBasicCfg :: BasicCfg
  , _geLogEnv   :: LogEnv
  , _geKeyTable :: KeyTable
  }
makeClassy ''BasicEnv

instance HasLogEnv     BasicEnv where logEnv     = geLogEnv
instance HasKeyTable   BasicEnv where keyTable   = geKeyTable

type CanBasic m env = ( EnvUIO m env, HasKeyTable env
                      , HasLogEnv env, HasBasicEnv env)

{- NOTE: invocation -----------------------------------------------------------}

-- -- | The full 'Invocation' with which KMonad is called
-- data Invocation = Invocation
--   { _iBasicCfg :: BasicCfg -- ^ Basic configuration settings
--   , _task      :: Task      -- ^ What task we are instructed to perform
--   } deriving (Eq, Show)
-- makeClassy ''Invocation
