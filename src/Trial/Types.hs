-- |

module Trial.Types where

import KMonad.Prelude

-- | Setting up the barest of streams, to start experimenting a bit.

-- | An interval in time, positive only, in milliseconds.
newtype Interval = Interval { unInterval :: Word64 }
  deriving (Eq, Ord, Num, Bounded, Enum)
instance Show Interval where show = show . unInterval


-- | A functor for events that occurs after some time.
newtype Timed a = Timed { unTimed :: (a, Interval) }

-- | Infinite sausage of many bites
newtype Sausage a = Bite { bite :: (a, Sausage a) }

-- | Timestream: linear time with events occuring at points on that axis
--
-- NOTE: We model this internally as a sausage of bites, but I think it is
-- really important to let go of that detail. Think of sausages as chompable
-- sequences. Bite -> nomnomnom, Bite -> nomnomnom. They are discrete. Try to
-- think of Timestreams as mathematical axes or rays, with the stored events as
-- points on that line.
newtype Timestream a = Timestream { unTS :: Sausage (Timed a) }

-- CONTINUE HERE
instance (Ord a) => Semigroup (Timestream a) where
  (Timestream (Bite ((a, ta), as))) <> (Timestream (Bite (b, tb) bs))
    | ta < tb = Bite ((a, ta), as <> Bite (b, tb - ta) bs)
    | ta > tb = Bite ((b, tb), bs <> Bite (a, ta - tb) as)
    |  a <  b = Bite ((a, ta), (Bite (b, 0), as <> bs))
    |  a >  b = Bite ((b, tb), (Bite (a, 0), as <> bs))




-- | A Segment of a timestream
type Segment a = Timed [Timed a]


{-
NOTE: So how do I proceed?

- Write monoid instance of timeseries


-}
