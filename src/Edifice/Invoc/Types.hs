module Edifice.Invoc.Types

where

import Preface

import Art
import Util.Configurable

-- | The instruction being passed to KMonad
data Task
  = Run       -- ^ Run KMonad as a keyboard remapper
  | Discover  -- ^ Print out information about an input device
  | ParseTest -- ^ Test if a configuration file parser without error
  deriving (Eq, Show)
makeClassyPrisms ''Task

-- | The property of possesing a Task
class HasTask a where task :: Lens' a Task
instance HasTask Task where task = id

-- | A collection of the task and all changes to the default configurations
data Invoc = Invoc
  { _iTask      :: Task
  , _logChanges :: Changes LogCfg
  }
makeClassy ''Invoc

instance HasTask Invoc where task = iTask
instance Default Invoc where def = Invoc Run def
instance Display Invoc where textDisplay = view description

instance HasDescription Invoc where
  description = to $ \i -> unlines . concat $
    [ ["task: " <> (tshow $ i^.task) ]
    , map (("log: " <>) . view description) $ i^.logChanges
    ]
