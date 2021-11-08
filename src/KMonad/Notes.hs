-- |

module Notes where

import Preface
import KMonad.Util

-- import Data.Stream

-- type Sausage = Stream

data Sausage a = Bite a (Sausage a)

data ChunkSize
  = Count Int
  | Duration Interval

instance Foldable Sausage where
  foldMap f (Bite x rst) = f x <> foldMap f rst

-- | Intervals are *positive only* Ms seconds
--
-- NOTE: currently can be negative, to be fixed
newtype Interval = Interval Ms

class HasInterval a where interval :: Getter a Interval

-- | Get the entries that occur in the next interval
bite :: (HasInterval a) => Sausage a -> ChunkSize -> ([a], Sausage a)
bite = undefined


-- | Time is a sausage of acruing intervals
type Time = Sausage Interval




{-
Build a worm that eats and poops

-- | The functor that maps inputs and context into behavior is the mind of the worm

We start at 0,0. There are no open references to the past or the future.

We wait for something to happen.

A key-event has been deliverd to the edifice and brought to us. Therefore it
follows the laws of:
- sequentiality
- Contains a reference to how long since we started waiting.
- Follows the laws of alternation, and is therefore guaranteed to be a press

We feed the key-event to our piranhas. They all update their context at the same
time, and produce a monoid of interests.

This monoid expresses the desires of the system, it can ask:
- Let me know if some (optionally expiring) predicate is matched and if so, let
  me know with what.
- Collecting these desires we can find the first decision point. I.e. out of all
  the desires, there is one that will terminate first.
- Then we either wait for the shortest timeout, or else indefinetly for the next
  keyevent.
- If no keyevent occurs within the timeout, the context gets updated with
  FailedMostUrgent, which recomputes the context
- We then wait again for the shortest timeout, or indefinetly
- The context of interests resets at every update, interests have to keep being
  reasserted (possibly with diminishing timeouts).
- -> Does this mean: interests have an update rule?

- Interests can also express a desire to be the first to perform some action in
  the future. I.e.: I want to emit control in 500s, but maybe something happens
  to make me change my mind.
- These interests also get updated with every event.

SOME FUNCTOR
- The interests are layered like an onion: You can always insert a layer on top.
- We deal with releases differently than presses, we only catch presses, and a
  press always comes with its own release-action

Write both an inchworm-in-IO and inchworm-test






-}

{-

Then we can write interesting expressions like:

-}



{-

Every bite has:

How do I reason about, a time before, a time after?

Bite c (Bite d (Bite c))

We are a list with some metadata.

The metadata of the key-signal is:
- alternating
- timestamp
- sequential in time

There is an interesting language (semantics) we can express on top of this signal:

We can express an interest in what has happened before
We can express an interest in what will happen
Depending on occurences we adjust our interests
We may issue instructions that get relayed to the Edifice

* a
* some data-type
* a *lazy* time to next value -> reading it causes a execution to pause until it is resolved


-}

data LogCtx = Yes

-- | Input instructions received from the outside world
data Input
  = KeyRecv Keycode
  | Shutdown
  | Reset

-- | Inputs can calculate how long back the previous bite was
newtype Since = Since Ms
newtype Until = Until Ms

-- | Output instructions to be carried to the edifice to be performed on the outside world
data Output
  = KeySend Keycode
  | Log LogCtx Text
  | ParseReport LogCtx



-- Then:
type Ingest  = Sausage (Since, Input)
type Expulse = Sausage (Until, Output)


{-

I have 2 slightly different sausages, the first I receive from the OS, and it
contains a sequence of instructions. The instructions are sequential and
timestamped.

Actually, I have 2 of the same kinds of sausages, but two different ways of interacting.

The first sausage is made by repeatedly calling some generating function
The other sausage is made by some resolution calculation over a cloud of references
But they are both sequential toggles of key-states, no they are not.

One is a sequence of input events:
- KeyEvent
- Signal
- Socket

or... is just a sequence of KeyEvents, and we deal with Signal and Socket in the
IO-mess. But there might be non-key-event instructions like:
- Shutdown -> drop all your references, progress the waiter up to the now, and exit.
- Reset    -> drop all your references, progress the waiter up to the now, reinit.
- Parse    -> parse my cfg and tell me what you think

Whether we work on a sausage of only key-events or some sum-factor can be
decided in the future.

The other is a sequence of output events:
- KeyEvent
- Logging
- IO-calls



-}
