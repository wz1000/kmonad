-- |

module Evdev.Types

where

import Preface

import System.Keyboard.Types
import System.Keyboard.Types.Linux


{- SECTION: Scratch -----------------------------------------------------------}




{- SUBSECTION: Linux ----------------------------------------------------------}

-- | The configuration record for evdev key-input on Linux
--
-- If no path is passed to Evdev, we try to autodetect the keyboard by matching
-- the first keyboard under @/dev/input/by-path@ that ends in @kbd@
newtype EvdevCfg = EvdevCfg
  { _evdevPath :: Maybe FilePath -- ^ The path to the device-file
  } deriving (Eq, Show)
makeClassy ''EvdevCfg

instance Default EvdevCfg where def = EvdevCfg Nothing
