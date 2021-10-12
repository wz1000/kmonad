{-
Place the following here:

- Types
- Classes
- Instances
- Smart constructors
- Basic lenses

-}


-- |

module System.Keyboard.Types where

import Prelude



{- NOTE: Generic ---------------------------------------------------------------
The central aim of this library is to provide OS-agnostic representation and
semantics for keyboard operations, across Windows, Mac, and Linux. All 3
operating systems function differently in how they represent standard events,
and how they represent keys.

The following section contains the generic representation that lies at the core
of this library:

We represent only presses and releases (not Linux's repeat events).

We represent keycodes as Word64's, since out of the 3 OSes the largest type is
Mac's (Word32, Word32), which we can fit into a Word64.

OS-specific types are in their own sections below.
-------------------------------------------------------------------------------}


{- NOTE: Switch ---------------------------------------------------------------}

-- | The two different transitions a 2-state machine (a key) can make.
data Switch
  = Press
  | Release
  deriving (Eq, Ord, Show)
makeClassyPrisms ''Switch

_IsPress :: Iso' Switch Bool
_IsPress = iso (Press ==) (bool Press Release)

-- | A class describing how to get at a 'Switch' inside some structure
class HasSwitch a where switch :: Lens' a Switch

instance HasSwitch Switch where switch = id

instance Hashable Switch where
  hashWithSalt n = hashWithSalt n . (Press ==)

{- NOTE: Keycode --------------------------------------------------------------}

-- | The 'Keycode' type
newtype Keycode = Keycode { _uKeycode :: Word64 }
  deriving (Eq, Ord, Num, Enum, Hashable)
makeLenses ''Keycode

instance Show Keycode where show = showHex . _uKeycode

-- | A class describing how to get at a 'Keycode' inside some structure
class HasKeycode a where keycode :: Lens' a Keycode

instance HasKeycode Keycode where keycode = id

{- NOTE: KeySwitch ------------------------------------------------------------}

-- | The 'KeySwitch' type describing a state-change of some key
newtype KeySwitch = KeySwitch { _uKeySwitch :: (Switch, Keycode) }
  deriving (Eq, Ord, Hashable)
makeLenses ''KeySwitch

instance HasSwitch  KeySwitch where switch  = uKeySwitch . _1
instance HasKeycode KeySwitch where keycode = uKeySwitch . _2

-- | Constructor for 'KeySwitch' data
mkKeySwitch :: Switch -> Keycode -> KeySwitch
mkKeySwitch = curry KeySwitch

{- NOTE: Time ------------------------------------------------------------------

We do not represent event-times in the generic representation, leaving people to
use (Time, KeySwitch) tuples (or similar datasructures) to represent events at
particular times.

For those OS-level events that do carry time information, we will set this to
system-time 0 when constructed, but provide a lens into that system-time value
to allow for manual access to change the time from 0. Furthermore, events that
are read in IO from the OS will capture and store the time data in the family of
Raw types.

-}

-- | A class describing how to get at a 'SystemTime' inside some structure.
class HasSystemTime a where
  systemTime :: Lens' a SystemTime

  _s :: Lens' a Int64
  _s = systemTime . lens systemSeconds (\t s -> t { systemSeconds = s })

  _ns :: Lens' a Word32
  _ns = systemTime . lens systemNanoseconds (\t ns -> t { systemNanoseconds = ns })

instance HasSystemTime SystemTime where
  systemTime = id

{- NOTE: Linux -----------------------------------------------------------------

Linux signals events using a packet of data containing 5 values (see LinRawEvent
for documentation). In addition to the keycode-encoding, linux key event
handling differs from mac and windows in the following:

1. It includes a timestamp

We deal with this by representing this field in the record, but always setting
it to 0 for our internal representations. So for our generic encoding, it is
meaningless. We do however fill these fields with the correct data when we read
events from the kernel, in case someone wants to build on top of this in their
own app. To provide the ability to read these times, and to set times on
user-generated linux events we provide a lens into these fields.

This way we can forget about timing for our own uses, but allow people to still
access timing information if they want to use this library for their own
purposes.

2. It allows for repeat events in addition to press and release events

We deal with this by creating a 2nd type of event 'LinRepeatEvent', in addition
to 'LinKeyEvent', that represents repeat events. We don't deal with these any
further in our generic representation, but provide a KeyIO configuration setting
that allows the relaying or ignoring of repeat events.

3. It uses a sync event to signal driver-updates to the kernel

We deal with this by creating a 3d type of event 'LinSyncEvent'. We don't deal
with these any further in our generic representation, but provide a KeyIO
configuration setting that on the reader toggles the relaying of sync events,
and on the writer allows either manual or automatic syncing.

For more information on the the linux event representations, see:
https://www.kernel.org/doc/Documentation/input/input.txt

Linux event packet:
- s    : system-time seconds
- ns   : system-time nanoseconds
- type : 1 for key-event, 0 for sync event
- code : keycode
- val  : 0 for release, 1 for press, 2 for repeat, (and 0 again for sync)

-------------------------------------------------------------------------------}

{- NOTE: LinKeycode -----------------------------------------------------------}

-- | A keycode by/for Linux
newtype LinKeycode = LinKeycode { _uLinKeycode :: Word16 }
  deriving (Eq, Ord, Num, Enum, Hashable)
makeLenses ''LinKeycode

instance Show LinKeycode where show = showHex . _uLinKeycode

-- | An Iso between 'LinKeycode' and 'Keycode'
--
-- Note that this isn't a true iso, 'Keycode' is a larger type than
-- 'LinKeycode'. However: for all keycodes that are generated by the Linux
-- kernel or are looked up by name, this is guaranteed to be an Iso.
_LinKeycode :: Iso' Keycode LinKeycode
_LinKeycode = iso to_ from_
  where to_   = LinKeycode . fi . _uKeycode
        from_ = Keycode . fi . _uLinKeycode

{- NOTE: LinRawEvent ----------------------------------------------------------}

-- | A record representing the structure of a Linux input event.
--
-- This packet is constructed in such a way that it can be directly serialized
-- and deserialized with the kernel interface. It should not be used directly,
-- however, since both sync events and key events are represented by these
-- packets.
data LinRawEvent = LinRawEvent
  { _linS    :: !Word64  -- ^ The seconds component of system time
  , _linNS   :: !Word64  -- ^ The nanoseconds component of system time
  , _linType :: !Word16  -- ^ The kind of event (we only use key and sync events)
  , _linCode :: !Word16  -- ^ The keycode indentifier of the key
  , _linVal  :: !Int32   -- ^ Whether a press, release, or repeat event
  } deriving (Show, Eq)
makeClassy ''LinRawEvent

-- | A packet of serializeable data representing a linux press or release event
newtype LinKeyEvent = LinKeyEvent { _uLinKeyEvent :: LinRawEvent }
  deriving (Show)
makeLenses ''LinKeyEvent

instance HasLinRawEvent LinKeyEvent where linRawEvent = uLinKeyEvent

instance Eq LinKeyEvent where
  a == b = (a^.linCode) == (b^.linCode) && (a^.linVal) == (b^.linVal)

-- | Smart constructor for 'LinKeyEvent's
mkLinKeyEvent :: Switch -> LinKeycode -> LinKeyEvent
mkLinKeyEvent s c = LinKeyEvent
  $ LinRawEvent 0 0 1 (_uLinKeycode c) (if s^._IsPress then 1 else 0)

instance HasSwitch LinKeyEvent where
  switch = lens
    (\l   -> bool Press Release (l^.linVal == 1))
    (\l s -> l & linVal .~ (if s^._IsPress then 1 else 0))

instance HasKeycode LinKeyEvent where
  keycode = lens
    (\l   -> l^.linCode.to (Keycode . fi))
    (\l c -> l & linCode .~ c^._LinKeycode.uLinKeycode)

-- | A packet of serializeable data representing a linux repeat event
newtype LinRepeatEvent = LinRepeatEvent { _uLinRepeatEvent :: LinRawEvent }
  deriving (Show, Eq)

-- | Smart constructor for 'LinRepeatEvent's
mkRepeatEvent :: LinKeycode -> LinRepeatEvent
mkRepeatEvent c = LinRepeatEvent $ LinRawEvent 0 0 1 (_uLinKeycode c) 2

-- | A packet of serializeable data representing a linux sync event
newtype LinSyncEvent = LinSyncEvent { _uLinSyncEvent :: LinRawEvent }
  deriving (Show, Eq)

-- | Smart constructor for 'LinSyncEvent's
mkLinSyncEvent :: LinSyncEvent
mkLinSyncEvent = LinSyncEvent $ LinRawEvent 0 0 0 0 0

-- | An Iso between 'KeySwitch' and 'LinKeyEvent'.
--
-- Note that this isn't a true iso, we discard some irrelevant information in
-- the transformation:
-- * timing information
-- * truncate keycode
--
-- However, all the information that is relevant to our generic mapping respects
-- the iso laws.
_LinKeyEvent :: Iso' KeySwitch LinKeyEvent
_LinKeyEvent = iso
  (\s -> mkLinKeyEvent (s^.switch) (s^.keycode._LinKeycode))
  (\l -> mkKeySwitch   (l^.switch) (l^.keycode))

{- NOTE: Windows ---------------------------------------------------------------
-------------------------------------------------------------------------------}

{- NOTE: Mac -------------------------------------------------------------------
-------------------------------------------------------------------------------}

{- NOTE: Names -----------------------------------------------------------------
-------------------------------------------------------------------------------}

{- NOTE: IO-types --------------------------------------------------------------
-------------------------------------------------------------------------------}
