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

{- NOTE: KeyIO tokens ---------------------------------------------------------}

-- | All available methods of capturing keyboards
data InputToken
  = Evdev (Maybe FilePath) -- ^ Linux evdev source with optional FilePath to device
  | LLHook                 -- ^ Windows low-level keyboard hook
  | IOKit (Maybe Text)     -- ^ Mac IOKit source with optional keyboard subset
  deriving (Eq, Show)
makeClassyPrisms ''InputToken

instance Default InputToken where
#if defined linux_HOST_OS
  def = Evdev Nothing
#elif defined darwin_HOST_OS
  def = IOKit Nothing
#elif defined mingw32_HOST_OS
  def = LLHook
#endif

-- | Full configuration describing how to acquire a keyboard
data InputCfg = InputCfg
  { _inputToken :: InputToken -- ^ Token describing how to acquire the keyboard
  , _startDelay :: Ms         -- ^ How long to wait before acquiring input keyboard
  } deriving (Eq, Show)
makeClassy ''InputCfg

instance Default InputCfg where
  def = InputCfg def 200

-- | All available methods of simulating keyboards
data OutputToken
  = Uinput (Maybe Text) (Maybe Text) -- ^ Linux uinput with a name and optional cmd
  | SendKeys                         -- ^ Windows @SendKeys@ based event injector
  | Ext                              -- ^ Mac dext/kext based event injector
  deriving (Eq, Show)
makeClassyPrisms ''OutputToken

instance Default OutputToken where
#if defined linux_HOST_OS
  def = Uinput (Just "KMonad simulated keyboard") Nothing
#elif defined darwin_HOST_OS
  def = Ext
#elif defined mingw32_HOST_OS
  def = SendKeys
#endif

-- | Full configuration describing how to simulate a keyboard
data OutputCfg = OutputCfg
  { _outputToken  :: OutputToken
  , _mayRepeatCfg :: KeyRepeatCfg
  } deriving (Eq, Show)
makeClassy ''OutputCfg

instance Default OutputCfg where
  def = OutputCfg def def

-- | Configuration options that are required for everything in KMonad
data GlobalCfg = GlobalCfg
  { _logLevel    :: LogLevel  -- ^ Logging level
  , _logSections :: Bool      -- ^ Used to enable section-breaks in logging
  , _keyLocale   :: KeyLocale -- ^ Table of name-keycode correspondences
  } deriving (Eq, Show)
makeClassy ''GlobalCfg

instance Default GlobalCfg where
  def = GlobalCfg LevelWarn True EnUS

-- | Config options pertinent to running a KMonad model
data ModelCfg = ModelCfg
  { _cmdAllow    :: Bool    -- ^ Whether to allow KMonad to call shell commands
  , _fallthrough :: Bool    -- ^ Whether to rethrow uncaught key events
  , _composeKey  :: Keyname -- ^ What button to use as a compose-key
  , _macroDelay  :: Ms      -- ^ How long to pause between macro-taps
  } deriving (Eq, Show)
makeClassy ''ModelCfg

instance Default ModelCfg where
  def = ModelCfg False True "ralt" 10

-- | Value describing where to look for a configuration file
-- - InConfig: '$XDG_CONFIG_HOME'/kmonad/filepath
-- - InHome: '$HOME'/filepath
-- - InRoot: filepath
--
-- Note that XDG_CONFIG_HOME on Linux and Mac is usually @~/.config@, on Windows
-- it's %APPDATA% (e.g. C:/Users/<user>/AppData/Roaming).
data CfgFile = InConfig FilePath | InHome FilePath | InRoot FilePath
  deriving (Eq, Show)

instance Default CfgFile where def = InConfig "keymap.kbd"

-- | Config options pertinent to loading and parsing a config file
newtype ParseCfg = ParseCfg
  { _cfgFile :: CfgFile -- ^ Path to the file to load
  } deriving (Eq, Show)
makeClassy ''ParseCfg

instance Default ParseCfg where
  def = ParseCfg def

{- NOTE: Different ways to invoke the @kmonad@ command ------------------------}

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
  , _rParseCfg  :: ParseCfg  -- ^ Cfg how to load config file
  } deriving (Eq, Show)

instance Default RunCfg where def = RunCfg def def def def

-- | Config describing how to run @kmonad discover@
--
-- To do this we need to:
-- 1. Grab KeyI
-- 2. Maybe parse a config file (to get at KeyI)
data DiscoverCfg = DiscoverCfg
  { _dInputCfg :: InputCfg
  , _dParseCfg :: ParseCfg
  } deriving (Eq, Show)

instance Default DiscoverCfg where def = DiscoverCfg def def

-- NOTE: 'ParseTest' does not have it own 'ParseTestCfg' because it *only* needs
-- a ParseCfg. If we ever end up including more into ParseTest, we should give
-- it its own cfg like the others.

-- | The instruction being passed to KMonad
data Task
  = Run RunCfg
  | ParseTest ParseCfg
  | Discover DiscoverCfg
  deriving (Eq, Show)

-- | The full 'Invocation' with which KMonad is called
data Invocation = Invocation
  { _globalCfg :: GlobalCfg -- ^ Global configuration settings
  , _task      :: Task      -- ^ What task we are instructed to perform
  } deriving (Eq, Show)

{- NOTE: Mods ------------------------------------------------------------------
We use Endo SomeCfg to collect 'modifictions' to our configurations in the
config and invocation parsing.
-------------------------------------------------------------------------------}
