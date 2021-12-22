-- |

module Edifice.Types.Task where

import Util

import Art

{- SECTION: Run ---------------------------------------------------------------}
-- | Config describing how to run @kmonad run@
--
-- To do this we need to:
-- 1. Parse a config file
-- 2. Grab both KeyI and KeyO
-- 3. Run the model
data RunCfg = RunCfg
  { -- _rModelCfg  :: ModelCfg  -- ^ Cfg how the model is run
    _rInputCfg  :: KeyICfg   -- ^ Cfg how to grab input
  , _rLocaleCfg :: LocaleCfg -- ^ Cfg how to relate keynames and keycodes
  , _rOutputCfg :: KeyOCfg   -- ^ Cfg how to generate output
  } deriving (Eq, Show)
makeClassy ''RunCfg

-- | Config describing how to run @kmonad discover@
data DiscoverCfg = DiscoverCfg
  { _dInputCfg    :: KeyICfg   -- ^ Config how to grab input
  , _dLocaleCfg   :: LocaleCfg -- ^ Config describing keyname/keycode correspondences
  , _dumpKeyTable :: Bool      -- ^ Flag indicating whether to dump table
  , _escapeExits  :: Bool      -- ^ Flag indicating whether to exit on escape
  , _checkConfig  :: Bool      -- ^ Whether whe should load the config-file
  } deriving (Eq, Show)
makeClassy ''DiscoverCfg

-- | The instruction being passed to KMonad
data Task
  = Run RunCfg           -- ^ Run KMonad as a keyboard remapper
  | Discover DiscoverCfg -- ^ Print out information about an input device
  | ParseTest            -- ^ Test if a configuration file parser without error
  deriving (Eq, Show)
makeClassyPrisms ''Task
