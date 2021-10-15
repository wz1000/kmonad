{-# LANGUAGE CPP #-}
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

import Foreign.Storable

{- NOTE: Basic -}

type Name = Text

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

_IsPress :: HasSwitch a => Getter a Bool
_IsPress = switch . to (Press ==)



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

-- | A class used to describe how to convert between things and 'Keycode's
--
-- Note that not all instances of this class follow the iso-laws completely. The
-- keycode types for linux and windows are smaller than the underlying type of
-- 'Keycode'.
--
-- As long as you map between 1 OSes Keycode and the generic Keycode the iso
-- laws are followed, but if you start converting between OS representations,
-- this will break.
--
-- So:
-- yes: LinKeycode -> Keycode -> LinKeycode
-- no:  MacKeycode -> Keycode -> LinKeycode -> Keycode -> MacKeycode
--
-- This Iso' exists to make working inside 1 OS easy. It is not meant to support
-- intelligent conversion or generalization between OSes.
class IsKeycode a where _Keycode :: Iso' a Keycode
instance IsKeycode Keycode where _Keycode = id

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

-- | A class used to describe how to convert between things and 'KeySwitch'es
--
-- Note that not all instances of this class follow the iso-laws completely.
-- LinKeyEvents, for example, contain timing information that is discarded.
-- However, we never use the timing information anywhere. So for all
-- semantically relevant information, the iso-laws are followed.
class IsKeySwitch a where _KeySwitch :: Iso' a KeySwitch
instance IsKeySwitch KeySwitch where _KeySwitch = id

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

instance IsKeycode LinKeycode where
  _Keycode = iso (Keycode . fi . _uLinKeycode) (LinKeycode . fi . _uKeycode)


{- NOTE: LinPacket ------------------------------------------------------------}

-- | A record representing the structure of a Linux input event.
--
-- This packet is constructed in such a way that it can be directly serialized
-- and deserialized with the kernel interface. It should not be used directly,
-- however, since both sync events and key events are represented by these
-- packets.
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

-- | A packet of serializeable data representing a linux press or release event
newtype LinKeyEvent = LinKeyEvent { _uLinKeyEvent :: LinPacket }
  deriving (Show)
makeLenses ''LinKeyEvent

instance HasLinPacket LinKeyEvent where linPacket = uLinKeyEvent

instance HasSwitch LinKeyEvent where
  switch = lens
    (\l   -> bool Press Release (l^.linVal == 1))
    (\l s -> l & linVal .~ (if s^._IsPress then 1 else 0))

instance HasKeycode LinKeyEvent where
  keycode = lens
    (\l   -> l^.linCode.to (Keycode . fi))
    (\l c -> l & linCode .~ c^.re _Keycode.uLinKeycode)

instance IsKeySwitch LinKeyEvent where
  _KeySwitch = iso
    (\l -> mkKeySwitch   (l^.switch) (l^.keycode))
    (\s -> let c_ = s^.keycode.re _Keycode.to _uLinKeycode
               v_ = if s^._IsPress then 1 else 0
           in LinKeyEvent $ LinPacket 0 0 1 c_ v_)

-- | A packet of serializeable data representing a linux repeat event
newtype LinRepeatEvent = LinRepeatEvent { _uLinRepeatEvent :: LinPacket }
  deriving (Show, Eq)
makeLenses ''LinRepeatEvent

instance HasLinPacket LinRepeatEvent where linPacket = uLinRepeatEvent

-- | Smart constructor for 'LinRepeatEvent's
mkLinRepeatEvent :: LinKeycode -> LinRepeatEvent
mkLinRepeatEvent c = LinRepeatEvent $ LinPacket 0 0 1 (_uLinKeycode c) 2

-- | A packet of serializeable data representing a linux sync event
newtype LinSyncEvent = LinSyncEvent { _uLinSyncEvent :: LinPacket }
  deriving (Show, Eq)
makeLenses ''LinSyncEvent

instance HasLinPacket LinSyncEvent where linPacket = uLinSyncEvent

-- | Smart constructor for 'LinSyncEvent's
mkLinSyncEvent :: LinSyncEvent
mkLinSyncEvent = LinSyncEvent $ LinPacket 0 0 0 0 0

{- NOTE: low-level API --------------------------------------------------------}

-- | Sum type of all events handled by the low-level Linux KeyIO API
data LowLinEvent
  = LowLinKeyEvent    LinKeyEvent
  | LowLinRepeatEvent LinRepeatEvent
  | LowLinSyncEvent   LinSyncEvent
  deriving (Show)

-- | Direct conversion between 'LowLinEvent' and 'LinPacket' for all possible
-- packets that we support.
instance IsLinPacket LowLinEvent where
  _LinPacket = iso to_ from_
    where
      to_ (LowLinKeyEvent e)    = e^.linPacket
      to_ (LowLinRepeatEvent e) = e^.linPacket
      to_ (LowLinSyncEvent e)   = e^.linPacket
      from_ p = if
        | p^.linType == 0 -> LowLinSyncEvent   . LinSyncEvent   $ p
        | p^.linVal  == 2 -> LowLinRepeatEvent . LinRepeatEvent $ p
        | otherwise       -> LowLinKeyEvent    . LinKeyEvent    $ p

instance HasLinPacket LowLinEvent where
  linPacket = _LinPacket

{- NOTE: Windows ---------------------------------------------------------------
The windows implementation is much simpler than the Linux implementation,
because we do not distinguish between Press/Release/Repeat/Sync events. We only
have key presses and releases, so we do not have to represent some intermediate
@Low@ type, all 'WinPacket's are automatically 'KeySwitch'es.
-------------------------------------------------------------------------------}

-- | In Windows we use 'Word32', the windows-native keycode type
newtype WinKeycode = WinKeycode { _uWinKeycode :: Word32 }
  deriving (Eq, Ord, Num, Enum, Hashable)
makeLenses ''WinKeycode

instance IsKeycode WinKeycode where
  _Keycode = iso (Keycode . fi . _uWinKeycode) (WinKeycode . fi . _uKeycode)

instance Show WinKeycode where show = showHex . _uWinKeycode

data WinPacket = WinPacket
  { _winVal  :: Word8  -- ^ 0:press, 1:release
  , _winCode :: Word32 -- ^ The keycode identifier of the key
  } deriving (Show, Eq)
makeClassy ''WinPacket

-- | Storable instance for WinPacket because we use it for IO
instance Storable WinPacket where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf    _ = 8 -- (1 + 3-padding) + 4
  peek p = WinPacket <$> peekByteOff p 0 <*> peekByteOff p 4
  poke p (WinPacket s c) = pokeByteOff p 0 s *> pokeByteOff p 4 c

instance HasSwitch WinPacket where
  switch = lens
    (\w   -> bool Press Release (w^.winVal == 0))
    (\w s -> w & winVal .~ (if s^._IsPress then 0 else 1))

instance HasKeycode WinPacket where
  keycode = lens
    (\w   -> w^.winCode.to (Keycode . fi))
    (\w c -> w & winCode .~ (c^.re _Keycode.uWinKeycode))

instance IsKeySwitch WinPacket where
  _KeySwitch = iso
    (\w -> mkKeySwitch (w^.switch) (w^.keycode))
    (\s -> let v_ = if s^._IsPress then 0 else 1
               c_ = s^.keycode.re _Keycode.to _uWinKeycode
           in WinPacket v_ c_)

{- NOTE: Mac -------------------------------------------------------------------
-------------------------------------------------------------------------------}

-- newtype MacKeycode = MacKeycode { _uMacKeycode :: (Word32, Word32) }
--   deriving (Eq, Ord, Num, Enum, Hashable)
-- makeLenses ''MacKeycode

{- NOTE: Names -----------------------------------------------------------------
-------------------------------------------------------------------------------}


{- NOTE: IO-types --------------------------------------------------------------
-------------------------------------------------------------------------------}

type Operation = Text
type OSName    = Text

-- NOTE This shouldn't live here
currentOS :: OSName
#if defined linux_HOST_OS
currentOS = "linux"
#elif defined mingw32_HOST_OS
currentOS = "windows"
#elif defined darwin_HOST_OS
currentOS = "mac"
#endif

data OSException = FFIWrongOS Operation OSName
  deriving Show

instance Exception OSException where
  displayException (FFIWrongOS action target) = unpack . mconcat $
    [ "Tried to '", action, "' on <", currentOS
    , ">. But this is only supported on <", target, ">" ]
