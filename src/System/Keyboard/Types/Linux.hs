-- |

module System.Keyboard.Types.Linux where

import Preface

import System.Keyboard.Types


{- SECTION: Linux --------------------------------------------------------------

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

Whereas Mac and Windows signal repeat events by emitting more presses, Linux has
a special event for this. We represent these events as a newtype over packet,
and use them in the 'Uinput' module but never represent 'Repeat' events anywhere
else.

3. It uses a sync event to signal driver-updates to the kernel

We deal with this by creating a 3d type of event 'LinSyncEvent'. We don't deal
with these any further in our generic representation, but use them in the
'Uinput' module.

For more information on the the linux event representations, see:
https://www.kernel.org/doc/Documentation/input/input.txt
-------------------------------------------------------------------------------}

{- SUBSECTION: Keycode --------------------------------------------------------}

-- | How linux represents keycodes
newtype LinKeycode = LinKeycode { _uLinKeycode :: Word16 }
  deriving (Eq, Ord, Enum, Hashable)
makeLenses ''LinKeycode

instance Show LinKeycode where show = showHex . _uLinKeycode

instance IsKeycode LinKeycode where
  _Keycode = iso (Keycode . fi . _uLinKeycode) (LinKeycode . fi . _uKeycode)

{- SUBSECTION: Packets --------------------------------------------------------}

-- | A record representing the structure of a Linux input event.
--
-- This packet is constructed in such a way that it can be directly serialized
-- and deserialized with the kernel interface.
data LinPacket = LinPacket
  { _linS    :: !Word64  -- ^ The seconds component of system time
  , _linNS   :: !Word64  -- ^ The nanoseconds component of system time
  , _linType :: !Word16  -- ^ 0:sync, 1:key-event
  , _linCode :: !Word16  -- ^ The keycode indentifier of the key
  , _linVal  :: !Int32   -- ^ 0:release, 1:press, 2:release
  } deriving (Show, Eq)
makeClassy ''LinPacket

class IsLinPacket a where _LinPacket :: Iso' a LinPacket
instance IsLinPacket LinPacket where _LinPacket = id

-- NOTE: The code is only a keycode for Key and Repeat events
instance HasKeycode LinPacket where
  keycode = lens
    (\l   -> l^.linCode.to (Keycode . fi))
    (\l c -> l & linCode .~ c^.re _Keycode.uLinKeycode)

-- | A packet of serializeable data representing a linux press or release event
newtype LinKeyEvent = LinKeyEvent { _uLinKeyEvent :: LinPacket }
  deriving (Show)
makeLenses ''LinKeyEvent

instance HasSwitch LinKeyEvent where
  switch = lens
    (\l   -> bool Release Press (l^.linVal == 1))
    (\l s -> l & linVal .~ (bool 0 1 $ has _Press s))
instance HasLinPacket LinKeyEvent where linPacket = uLinKeyEvent

instance HasKeycode LinKeyEvent where keycode = uLinKeyEvent.keycode

instance AsKeySwitch LinKeyEvent where
  _KeySwitch = iso
    (\l -> mkKeySwitch (l^.switch) (l^.keycode))
    (\s -> LinKeyEvent (LinPacket 0 0 1 undefined undefined)
           & switch  .~ (s^.switch)
           & keycode .~ (s^.keycode))

-- | A packet of serializeable data representing a linux repeat event
newtype LinRepeatEvent = LinRepeatEvent { _uLinRepeatEvent :: LinPacket }
  deriving (Show, Eq)
makeLenses ''LinRepeatEvent

instance HasLinPacket LinRepeatEvent where linPacket = uLinRepeatEvent
instance HasKeycode   LinRepeatEvent where keycode   = linPacket.keycode

-- | Smart constructor for 'LinRepeatEvent's
mkLinRepeatEvent :: LinKeycode -> LinRepeatEvent
mkLinRepeatEvent c = LinRepeatEvent $ LinPacket 0 0 1 (_uLinKeycode c) 2

-- | A packet of serializeable data representing a linux sync event
newtype LinSyncEvent = LinSyncEvent { _uLinSyncEvent :: LinPacket }
  deriving (Show, Eq)
makeLenses ''LinSyncEvent

instance HasLinPacket LinSyncEvent where linPacket = uLinSyncEvent

-- | A packet of serializeable data representing a linux scancode event
newtype LinScanEvent = LinScanEvent { _uLinScanEvent :: LinPacket }
  deriving (Show, Eq)
makeLenses ''LinScanEvent

instance HasLinPacket LinScanEvent where linPacket = uLinScanEvent

-- | Smart constructor for 'LinSyncEvent's
mkLinSyncEvent :: LinSyncEvent
mkLinSyncEvent = LinSyncEvent $ LinPacket 0 0 0 0 0

{- SUBSECTION: Events ---------------------------------------------------------}

-- | Sum type of all events handled by the low-level Linux KeyIO API
data LowLinEvent
  = LowLinKeyEvent    LinKeyEvent
  | LowLinRepeatEvent LinRepeatEvent
  | LowLinScanEvent   LinScanEvent
  | LowLinSyncEvent   LinSyncEvent
  deriving (Show)
makeClassyPrisms ''LowLinEvent

-- | Direct conversion between 'LowLinEvent' and 'LinPacket' for all possible
-- packets that we support.
instance IsLinPacket LowLinEvent where
  _LinPacket = iso to_ from_
    where
      to_ (LowLinKeyEvent e)    = e^.linPacket
      to_ (LowLinRepeatEvent e) = e^.linPacket
      to_ (LowLinSyncEvent e)   = e^.linPacket
      to_ (LowLinScanEvent e)   = e^.linPacket
      from_ p = if
        | p^.linType == 0 -> LowLinSyncEvent   . LinSyncEvent   $ p
        | p^.linVal  == 2 -> LowLinRepeatEvent . LinRepeatEvent $ p
        | p^.linType == 4 -> LowLinScanEvent   . LinScanEvent   $ p
        | otherwise       -> LowLinKeyEvent    . LinKeyEvent    $ p

instance HasLinPacket LowLinEvent where
  linPacket = _LinPacket

instance AsKeySwitch LowLinEvent where
  _KeySwitch = _LowLinKeyEvent._KeySwitch
