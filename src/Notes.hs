-- |

module Notes where

import Preface

type Ms = Int

--------------------------------------------------------------------------------
-- IO data repr
--
-- Sausage: an infinite series of events.
-- TimeSeries: continuous time axis with events occuring somewhere
-- TimeSegment: a finite time-series
--

data Sausage a = Bite a (Sausage a)

-- | A 'TimeSeries' gets modeled as a series of sequential chunks, each chunk
-- containing how long until it happens.
--
-- This is actually an implementation detail: I should write the logic of
-- timestreams, and then pick some implementation, but not expose it.
newtype TimeSeries a = Sausage (a, Ms)
newtype TimeSegment a = TimeSegment [(a, Ms)]


-- So what are the important operations on timestreams?

-- | merge 2 time-series into 1, this should probably be: (<>)
merge :: TimeSeries a -> TimeSeries a -> TimeSeries a
merge = undefined

-- | return the next item, how long until it happens, and the rest of the timeseries
next :: TimeSeries a -> (a, Ms, TimeSeries a)
next = undefined

-- | return a TimeSegment of the occurences in some interval, returning the
-- continuation after the interval has passed.
within :: Ms -> TimeSeries a -> (TimeSegment a, TimeSeries a)
within = undefined


--------------------------------------------------------------------------------
-- Model data repr
--
-- Some kind of cloud of references, a vision of what happened in the past, the
-- ability to express intentions for the future.
--
-- ^^ actually: that is the 'computer', how we turn our model representation
-- into a series of output commands. This is not the Functor describing the
-- computation.
--
-- The actual model representation is: a sequence of nesting contexts
--
--


--------------------------------------------------------------------------------
-- Beyond model *data*



-- Stuttgart: anschluss 855 von Gl. 4
