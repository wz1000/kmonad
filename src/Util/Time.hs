-- | A small collection of time utilities

module Util.Time
  ( Time
  , HasTime(..)
  , Ms
  , _Ms

  , tDiff

  , getNow
  , withNow
  , wait
  )

where

import Preface

import Data.Time.Clock.System

import qualified RIO.Time as T
import qualified Data.Time.Clock.System as T

{- SECTION: Types -------------------------------------------------------------}

-- | A point in time
newtype Time  = Time  { _uTime  :: T.UTCTime }
  deriving (Eq, Ord, Show)
makeWrapped ''Time

-- | The property of containing some time-value
class HasTime a where time :: Lens' a Time

instance HasTime Time where time  = id

-- | A vector forward in time, in ms.
newtype Ms = Ms { _uMs :: Natural}
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral, Display)
makeWrapped ''Ms

{- SECTION: Lenses ------------------------------------------------------------}

_UTCTime :: Iso' Time T.UTCTime
_UTCTime = _Wrapped

-- | Easy conversion between Time and SystemTime
_SystemTime :: Iso' Time SystemTime
_SystemTime = _Wrapped . iso utcToSystemTime systemToUTCTime

_Ms :: Integral a => Iso' Ms a
_Ms = _Wrapped . iso fi fi

-- | Lens into the seconds field of a 'SystemTime'
_s :: Lens' SystemTime Int64
_s = lens systemSeconds (\t s -> t { systemSeconds = s })

-- | Lens into the nanoseconds field of a 'SystemTime'
_ns :: Lens' SystemTime Word32
_ns = lens systemNanoseconds (\t ns -> t { systemNanoseconds = ns })

{- SECTION: Operations --------------------------------------------------------}

-- | Calculate how much time has elapsed between 2 time points
tDiff :: ()
  => SystemTime   -- ^ The earlier timepoint
  -> SystemTime   -- ^ The later timepoint
  -> Ms           -- ^ The time in milliseconds between the two
tDiff a b = let
  a' = T.systemToUTCTime a
  b' = T.systemToUTCTime b
  d  = T.diffUTCTime b' a'
  in round $ d * 1000

{- SECTION: IO ----------------------------------------------------------------}

-- | Return the current time
getNow :: IO m => m Time
getNow = Time <$> T.getCurrentTime

-- | Pause computation for some milliseconds
wait :: IO m => Ms -> m ()
wait = threadDelay . (1000*) . fromIntegral

-- | Run a function that takes a time on the current time
withNow :: IO m => (Time -> a) -> m a
withNow = (<$> getNow)
