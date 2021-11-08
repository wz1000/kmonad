-- |

module Data.TimeStream where


import Preface

{- SECTION: Types -------------------------------------------------------------}

-- | The delayed type, containing some packet of data, and an Ms value
-- indicating some natural delay.
newtype Delayed a = Delayed { _uDelayed :: (Ms, a) }
  deriving (Eq, Show, Ord, Functor)

-- | A sequence of delayed values, where the delay is interpreted as 'time
-- between previous value and this one.'
type TimeStream a = [Delayed a]


runPaused :: IO m => (a -> b) -> TimeStream a -> m [b]
runPaused = undefined

-- runPausedM :: IO m => (a -> m b) -> TimeStream a ->

readTimeStream :: IO m => m a -> m (TimeStream a)
readTimeStream = undefined
