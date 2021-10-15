{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module Prelude
  ( module X

  , fi
  , _fi
  , showHex
  , time0
  )
where

-- For reexport
import Control.Lens           as X
import Control.Lens.Extras    as X
import Control.Exception.Lens as X
import Data.Default           as X
import Data.Hashable          as X (Hashable(..))
import Data.Time.Clock.System as X (SystemTime(..), getSystemTime)
import RIO.Text               as X ( pack, unpack )
import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- The following line is required for newer stack releases.
    -- This is also the reason for the OPTIONS_GHC pragma
  , (^..), (^?), preview, (%~), (.~)

    -- Some stuff I'd rather default to Text
  , unlines, lines
  )

-- For util
import qualified Numeric as N (showHex)

-- | The origin of SystemTime
time0 :: SystemTime
time0 = MkSystemTime 0 0

-- | Show an integral as the hex-string that would construct it.
showHex :: (Integral a, Show a) => a -> String
showHex = ("0x" <>) . ($ "") . N.showHex

-- | Used so often a shorthand is nice
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

_fi :: (Integral a, Num b) => Getter a b
_fi = to fromIntegral
