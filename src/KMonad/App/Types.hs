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

import Preface

import KMonad.Util.Time
import KMonad.App.Cmds
import KMonad.App.Locale
import KMonad.App.Logging

import System.Keyboard

import UnliftIO.Directory
import qualified RIO.Text as T
import RIO.FilePath

import System.IO (putStrLn)



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
  { _fallthrough :: Bool   -- ^ Whether to rethrow uncaught key events
  , _macroDelay  :: Ms     -- ^ How long to pause between macro-taps
  } deriving (Eq, Show)
makeClassy ''ModelCfg

instance Default ModelCfg where def = ModelCfg True 10


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
  , _rLocaleCfg :: LocaleCfg -- ^ Cfg how to relate keynames and keycodes
  , _rOutputCfg :: OutputCfg -- ^ Cfg how to generate output
  } deriving (Eq, Show)
makeClassy ''RunCfg

instance Default RunCfg where def = RunCfg def def def def

instance HasModelCfg  RunCfg where modelCfg  = rModelCfg
instance HasInputCfg  RunCfg where inputCfg  = rInputCfg
instance HasLocaleCfg RunCfg where localeCfg = rLocaleCfg
instance HasOutputCfg RunCfg where outputCfg = rOutputCfg

-- | Config describing how to run @kmonad discover@
data DiscoverCfg = DiscoverCfg
  { _dInputCfg    :: InputCfg  -- ^ Config how to grab input
  , _dLocaleCfg   :: LocaleCfg -- ^ Config describing keyname/keycode correspondences
  , _dumpKeyTable :: Bool      -- ^ Flag indicating whether to dump table
  , _escapeExits  :: Bool      -- ^ Flag indicating whether to exit on escape
  , _checkConfig  :: Bool      -- ^ Whether whe should load the config-file
  } deriving (Eq, Show)
makeClassy ''DiscoverCfg

instance Default DiscoverCfg where
  def = DiscoverCfg def def False True False

instance HasInputCfg DiscoverCfg where inputCfg = dInputCfg
instance HasLocaleCfg DiscoverCfg where localeCfg = dLocaleCfg

{- SECTION: task --------------------------------------------------------------}

-- | The instruction being passed to KMonad
data Task
  = Run RunCfg           -- ^ Run KMonad as a keyboard remapper
  | Discover DiscoverCfg -- ^ Print out information about an input device
  | ParseTest            -- ^ Test if a configuration file parser without error
  deriving (Eq, Show)
makeClassyPrisms ''Task

instance Default Task where def = Run def

class HasTask a where task :: Lens' a Task
instance HasTask Task where task = id

-- TODO: Generalize the next few traversals

-- | A traversal over an 'InputCfg' stored in a 'Task'
--
-- i.e. How to get at maybe an InputCfg in a Task
taskInputCfg :: HasTask s => Traversal' s InputCfg
taskInputCfg f s = case s^.task of
  ParseTest  -> pure s
  Run c      -> (\ic -> s & task._Run.inputCfg .~ ic)      <$> (f $ c^.inputCfg)
  Discover c -> (\ic -> s & task._Discover.inputCfg .~ ic) <$> (f $ c^.inputCfg)
{- ^NOTE: This one was difficult because both Run and Discover have an InputCfg -}

-- | A traversal over an 'OutputCfg' stored in a 'Task'
taskOutputCfg :: HasTask s => Traversal' s OutputCfg
taskOutputCfg = task._Run.outputCfg

-- | A traversal over a 'ModelCfg' stored in a 'Task'
taskModelCfg :: HasTask s => Traversal' s ModelCfg
taskModelCfg = task._Run.modelCfg

-- | A traversal over a 'LocaleCfg' stored in a 'Task'
--
-- i.e. How to get at maybe an InputCfg in a Task
taskLocaleCfg :: HasTask s => Traversal' s LocaleCfg
taskLocaleCfg f s = case s^.task of
  ParseTest  -> pure s
  Run c      -> (\lc -> s & task._Run.localeCfg .~ lc)      <$> (f $ c^.localeCfg)
  Discover c -> (\lc -> s & task._Discover.localeCfg .~ lc) <$> (f $ c^.localeCfg)
{- ^NOTE: This one was difficult because both Run and Discover have an InputCfg -}

{- SECTION: basic cfg ---------------------------------------------------------}

-- | Configuration options that are required for everything in KMonad
data RootCfg = RootCfg
  { _rLogCfg  :: LogCfg         -- ^ Logging cfg
  , _rCmdsCfg :: CmdsCfg        -- ^ Hooks and commands cfg
  , _bcTask   :: Task           -- ^ The instruction passed to KMonad
  , _cmdAllow :: Bool           -- ^ Whether to allow KMonad to call shell commands
  , _cfgFile  :: Maybe FilePath -- ^ Where to look for a config file
  } deriving (Eq, Show)
makeClassy ''RootCfg

instance Default RootCfg where
  def = RootCfg
    { _rLogCfg  = def
    , _rCmdsCfg = def
    , _bcTask   = def
    , _cmdAllow = False
    , _cfgFile  = Nothing
    }

instance HasLogCfg  RootCfg where logCfg  = rLogCfg
instance HasCmdsCfg RootCfg where cmdsCfg = rCmdsCfg
instance HasTask RootCfg where task = bcTask



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
