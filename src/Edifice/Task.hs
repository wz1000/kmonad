module Edifice.Task
  ( Task(..)
  , module X )
where

import Edifice.Task.Discover  as X

import Edifice.Task.ParseTest as X
import Edifice.Task.Run       as X

import Preface as X

data Task
  = Run RunCfg           -- ^ Run KMonad as a keyboard remapper
  | Discover DiscoverCfg -- ^ Print out information about an input device
  | ParseTest            -- ^ Test if a configuration file parser without error
  deriving (Eq, Show)
makeClassyPrisms ''Task

instance Default Task where def = Run def

class HasTask a where task :: Lens' a Task
instance HasTask Task where task = id
