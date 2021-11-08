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
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import System.Keyboard.Prelude

import Data.Bits
import Foreign.Storable

import GHC.Enum (Enum(..))

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

{- SECTION: Basic types -------------------------------------------------------}

{- SUBSECTION: Descriptive names ----------------------------------------------}

-- | Description that only ever gets printed, never searched or compared
type Description = Text
-- | Text used as a name, names are assumed to be unique
type Name        = Text
-- | Name assigned to a key
type Keyname     = Name


{- SUBSECTION: Switch ---------------------------------------------------------}

-- | The two different transitions a 2-state machine (a key) can make.
data Switch
  = Press
  | Release
  deriving (Eq, Ord, Show)
makeClassyPrisms ''Switch

-- | A class describing how to get at a 'Switch' inside some structure
class HasSwitch a where switch :: Lens' a Switch

instance HasSwitch Switch where switch = id

instance Hashable Switch where
  hashWithSalt s = hashWithSalt s . (Press ==)

-- | Simple getter from switch to bool
isPress :: HasSwitch a => Getter a Bool
isPress = to $ \a -> has _Press (a^.switch)

-- | Flip a switch inside some structure that has one
switched :: HasSwitch a => Iso' a a
switched = let f a = a & switch .~ (bool Release Press $ a^.isPress) in iso f f

{- SUBSECTION: Keycode --------------------------------------------------------}

-- | The 'Keycode' type
newtype Keycode = Keycode { _uKeycode :: Word64 }
  deriving (Eq, Ord, Enum, Num, Hashable)
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
instance IsKeycode Word64 where _Keycode = from uKeycode

{- SUBSECTION: KeySwitch ------------------------------------------------------}

-- | The 'KeySwitch' type describing a state-change of some key
newtype KeySwitch = KeySwitch { _uKeySwitch :: (Switch, Keycode) }
  deriving (Eq, Ord, Hashable, Show)
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
class AsKeySwitch a where _KeySwitch :: Prism' a KeySwitch
instance AsKeySwitch KeySwitch where _KeySwitch = id




{- SECTION: Windows ------------------------------------------------------------
The windows implementation is much simpler than the Linux implementation,
because we do not distinguish between Press/Release/Repeat/Sync events. We only
have key presses and releases, so we do not have to represent some intermediate
@Low@ type, all 'WinPacket's are automatically 'KeySwitch'es.
-------------------------------------------------------------------------------}

{- SUBSECTION: Keycode --------------------------------------------------------}

-- | In Windows we use 'Word32', the windows-native keycode type
newtype WinKeycode = WinKeycode { _uWinKeycode :: Word32 }
  deriving (Eq, Ord, Enum, Hashable)
makeLenses ''WinKeycode

instance IsKeycode WinKeycode where
  _Keycode = iso (Keycode . fi . _uWinKeycode) (WinKeycode . fi . _uKeycode)

instance Show WinKeycode where show = showHex . _uWinKeycode

{- SUBSECTION: Packet ---------------------------------------------------------}

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
    (\w s -> w & winVal .~ (bool 1 0 $ has _Press s))

instance HasKeycode WinPacket where
  keycode = lens
    (\w   -> w^.winCode.to (Keycode . fi))
    (\w c -> w & winCode .~ (c^.re _Keycode.uWinKeycode))

instance AsKeySwitch WinPacket where
  _KeySwitch = iso
    (\w -> mkKeySwitch (w^.switch) (w^.keycode))
    (\s -> let v_ = bool 1 0 $ has (switch._Press) s
               c_ = s^.keycode.re _Keycode.to _uWinKeycode
           in WinPacket v_ c_)


{- SECTION: Mac ---------------------------------------------------------------}

{- SUBSECTION: Keycode --------------------------------------------------------}

-- TODO: Write a better Show for mac
newtype MacKeycode = MacKeycode { _uMacKeycode :: (Word32, Word32) }
  deriving (Eq, Ord, Show)
makeLenses ''MacKeycode

instance Hashable MacKeycode where
  hashWithSalt s = hashWithSalt s . view _Keycode

instance IsKeycode MacKeycode where
  _Keycode = iso
    (Keycode    . (\(a, b) -> shiftL (fi a) 32 + (fi b)) . _uMacKeycode)
    (MacKeycode . (\c      -> (fi $ shiftR c 32, fi c))  . _uKeycode)

instance Enum MacKeycode where
  toEnum   = view (from _Keycode) . toEnum
  fromEnum = fromEnum . view _Keycode

{- SUBSECTION: Packet ---------------------------------------------------------}
data MacPacket = MacPacket
  { _macVal  :: Word64
  , _macCode :: (Word32, Word32)
  } deriving (Eq, Ord, Show)

instance Storable MacPacket where
  alignment _ = 4
  sizeOf    _ = 16
  peek ptr = do
    s <- peekByteOff ptr 0
    p <- peekByteOff ptr 8
    u <- peekByteOff ptr 12
    return $ MacPacket s (p, u)
  poke ptr (MacPacket s (p, u)) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 p
    pokeByteOff ptr 12 u


{- SECTION: OS-support types --------------------------------------------------}

-- | The different OSes that we support
data OS = Linux | Mac | Windows deriving (Eq, Ord, Show)

-- | The OS under which we are compiled
currentOS :: OS
#if defined linux_HOST_OS
currentOS = Linux
#elif defined darwin_HOST_OS
currentOS = Mac
#elif defined mingw32_HOST_OS
currentOS = Windows
#endif

-- | Exceptions thrown when trying to do something on the wrong OS
data OSException = FFIWrongOS Description OS
  deriving Show

instance Exception OSException where
  displayException (FFIWrongOS action target) = unpack . mconcat $
    [ "Tried to '", action, "' on <", tshow currentOS
    , ">. But this is only supported on <", tshow target, ">" ]


-- {- SECTION: naming keys across OSes -------------------------------------------}

-- -- | A record describing a correspondence between a semantic key and OS codes
-- data KeyCongruence = KeyCongruence
--   { _keyName        :: Keyname
--   , _shiftedName    :: Maybe Keyname
--   , _keyDescription :: Text
--   , _keyLin         :: Maybe LinKeycode
--   , _keyMac         :: Maybe MacKeycode
--   , _keyWin         :: Maybe WinKeycode
--   } deriving (Eq, Show)
-- makeLenses ''KeyCongruence

-- instance Display KeyCongruence where
--   textDisplay k = mconcat
--     [ pad $ k^.keyName, " "
--     , pad $ fromMaybe "~~" (k^.shiftedName)            , " "
--     , pad $ maybe "~~" tshow (k^?keyLin._Just._Keycode), " "
--     , pad $ maybe "~~" tshow (k^?keyMac._Just._Keycode), " "
--     , pad $ maybe "~~" tshow (k^?keyWin._Just._Keycode), " "
--     , "(", k^.keyDescription, ")"
--     ]
--     where
--       pad = T.justifyLeft 10 ' '

-- -- | A table of keyname to keycode mappings across OSes
-- newtype KeyTable = KeyTable { _uKeyTable :: [KeyCongruence] }
--   deriving (Show, Eq)
-- makeLenses ''KeyTable

-- -- | Value indicating which key-locale to use.
-- data KeyTableCfg
--   = EnUS                 -- ^ Use the standard, built-in keytable
--   | CustomTable FilePath -- ^ Use a custom keytable, to be loaded from file
--   deriving (Eq, Show)

-- -- | A class describing how a keytable is stored.
-- class HasKeyTable a where keyTable :: Getter a KeyTable

-- instance HasKeyTable KeyTable where keyTable = id

-- type CanKeyTable m env = (MonadReader env m, HasKeyTable env)

-- -- | A simple Keyname to Keycode mapping
-- type KeyDict = M.HashMap Keyname Keycode


{- SECTION: Primary IO config -------------------------------------------------}


-- | Configuration for uinput keysink in Linux
data UinputCfg = UinputCfg
  { _vendorCode     :: !Int  -- ^ USB vendor code of the generated keyboard
  , _productCode    :: !Int  -- ^ USB product code of the generated keyboard
  , _productVersion :: !Int  -- ^ USB product version
  , _keyboardName   :: !Name -- ^ Name used to identify keyboard to OS
  } deriving (Eq, Show)
makeClassy ''UinputCfg

instance Default UinputCfg where
  def = UinputCfg
    { _vendorCode     = 0xFFFF
    , _productCode    = 0xFFFF
    , _productVersion = 0x0000
    , _keyboardName   = "Haskell simulated keyboard" }

{- SUBSECTION: Mac ------------------------------------------------------------}

-- | Configuration for Mac's IOKit input module
newtype IOKitCfg = IOKitCfg
  { _productStr :: Maybe Text -- ^ A string to restrict which keyboard to capture
  } deriving (Eq, Show)
makeClassy ''IOKitCfg

instance Default IOKitCfg where def = IOKitCfg Nothing

-- | Configuration for (k/d)ext keysink in Mac
data ExtCfg = ExtCfg deriving (Eq, Show)

instance Default ExtCfg where def = ExtCfg

{- SUBSECTION: Windows --------------------------------------------------------}

-- | Configuration for Window's LLHook input
data LLHookCfg = LLHookCfg deriving (Eq, Show)

instance Default LLHookCfg where def = LLHookCfg

-- | Configuration for Window's SendKeys output
data SendKeysCfg = SendKeysCfg deriving (Eq, Show)

instance Default SendKeysCfg where def = SendKeysCfg


{- SECTION: High-level KeyIO --------------------------------------------------}

{- SUBSECTION: key repeat -----------------------------------------------------}

-- | Settings that describe how to trigger key-repeat events
data KeyRepeatCfg = KeyRepeatCfg
  { _repeatDelay :: Int  -- ^ How many milliseconds before we start repeating
  , _repeatRate  :: Int  -- ^ How many milliseconds between repeat events
  } deriving (Eq, Show)
makeClassy ''KeyRepeatCfg

instance Default KeyRepeatCfg where
#if defined mingw32_HOST_OS
  {- NOTE: On Windows we *need* to provide key-repeat, because it doesn't happen
     automatically. On Linux's virtual console it doesn't either, but that is an
     edgecase that we should leave to users to configure. -}
  def = KeyRepeatCfg 300 100
#else
  {- NOTE: (maxBound :: Int) ms > 290 millenia... probably enough -}
  def = KeyRepeatCfg maxBound maxBound
#endif


{- SECTION: OS-unification ----------------------------------------------------}

{- SUBSECTION: Input ----------------------------------------------------------}

-- -- | All available methods of capturing keyboards
-- data InputToken
--   = Evdev  EvdevCfg  -- ^ Linux evdev source with optional FilePath to device
--   | LLHook LLHookCfg -- ^ Windows low-level keyboard hook
--   | IOKit  IOKitCfg  -- ^ Mac IOKit source with optional keyboard subset
--   deriving (Eq, Show)
-- makeClassyPrisms ''InputToken

-- instance Default InputToken where
-- #if defined linux_HOST_OS
--   def = Evdev def
-- #elif defined darwin_HOST_OS
--   def = IOKit def
-- #elif defined mingw32_HOST_OS
--   def = LLHook def
-- #endif

-- -- | High-level keyboard input access
-- newtype InputEnv = InputEnv { _uInputEnv :: IO KeySwitch }
-- makeLenses ''InputEnv

-- -- | Wrap some generator of AsKeySwitches in an InputEnv
-- mkInputEnv :: (AsKeySwitch a, MonadUnliftIO m) => m a -> m InputEnv
-- mkInputEnv m = do
--   u <- askRunInIO
--   let nxt = preview _KeySwitch <$> m >>= \case
--         Nothing -> nxt
--         Just a  -> pure a
--   pure . InputEnv . u $ nxt

-- class HasInputEnv a where keyI :: Getter a InputEnv

-- -- | Full configuration describing how to acquire a keyboard
-- data InputCfg = InputCfg
--   { _inputToken :: InputToken -- ^ Token describing how to acquire the keyboard
--   , _startDelay :: Int        -- ^ How many ms to wait before acquiring input keyboard
--   } deriving (Eq, Show)
-- makeClassy ''InputCfg

-- instance Default InputCfg where def = InputCfg def 200

-- newtype Src s = Src { _uSrc :: IO s }


-- -- | A class designating some configuration as being able to open a context in
-- -- which we can read key events from IO.
-- class (AsKeySwitch s) => CanWithInput cfg s where
--   withKeyInput :: forall m a. MonadUnliftIO m => cfg -> (Src s -> m a) -> m a

-- {- SUBSECTION: Output ---------------------------------------------------------}

-- -- | All available methods of simulating keyboards
-- data OutputToken
--   = Uinput   UinputCfg   -- ^ Linux @uinput@ with name and post-init cmd
--   | SendKeys SendKeysCfg -- ^ Windows @SendKeys@ based event injector
--   | Ext      ExtCfg      -- ^ Mac @dext/kext@ based event injector
--   deriving (Eq, Show)
-- makeClassyPrisms ''OutputToken

-- instance Default OutputToken where
-- #if defined linux_HOST_OS
--   def = Uinput def
-- #elif defined darwin_HOST_OS
--   def = Ext def
-- #elif defined mingw32_HOST_OS
--   def = SendKeys def
-- #endif
-- class HasOutputToken a where outputToken :: Lens' a OutputToken

-- instance HasOutputToken OutputToken where outputToken = id

-- -- | High-level keyboard output access
-- -- data KeyOutput = KeyOutput
-- --   { _emit   :: KeySwitch -> IO () -- ^ Emit a keyswitch to the OS
-- --   , _repeat :: Keycode   -> IO () -- ^ Signal repeat of keycode to OS
-- --   }

-- -- class HasKeyOutput a where keyO :: Getter a KeyOutput

-- -- class CanOpenKeyOutput cfg where
-- --   withKeyOutput :: forall m a. MonadUnliftIO m => cfg -> (KeyOutput -> m a) -> m a

-- -- | Full configuration describing how to simulate a keyboard
-- data OutputCfg = OutputCfg
--   { _cOutputToken  :: OutputToken
--   , _cKeyRepeatCfg :: KeyRepeatCfg
--   } deriving (Eq, Show)
-- makeClassy ''OutputCfg

-- instance Default OutputCfg where def = OutputCfg def def

-- instance HasOutputToken  OutputCfg where outputToken  = cOutputToken
-- instance HasKeyRepeatCfg OutputCfg where keyRepeatCfg = cKeyRepeatCfg


-- data Snk s c = Snk
--   { _emit   :: s -> IO ()
--   , _repeat :: c -> IO ()
--   }

{- NOTE:

The Traversal's type below is the same as:

inputSelector :: Applicative f
  => (Maybe Text -> f (Maybe Text))
  -> InputToken
  -> f InputToken

This is just how VanLaarhoven traversals work
-}


-- case i^?_
--   Just (Evdev (EvdevCfg t)) -> review _Evdev . EvdevCfg
--     . fmap unpack <$> f (pack <$> t)
--   _ -> pure i

-- | A traversal over the keyboard-selecting text inside an InputToken.
--
-- This can be used to view the selector like this:
-- >>> token^?inputSelector
-- Just "/dev/input/blabla"
--
-- Or set the selector like this:
-- >>> token & inputSelector .~ Just "/dev/input/foobar"
-- <some input token>
--
-- On Linux this corresponds to the device-file
-- On Mac this corresponds to the keyboard-name subset
-- On windows this is a NoOp
-- inputSelector :: AsInputToken s => Traversal' s (Maybe Text)
-- inputSelector f i = case i^?_InputToken of
--   Just (IOKit (IOKitCfg t)) -> review _IOKit . IOKitCfg <$> f t
--   Just (Evdev (EvdevCfg t)) -> review _Evdev . EvdevCfg
--     . fmap unpack <$> f (pack <$> t)
--   _ -> pure i









{- NOTE: General input types --------------------------------------------------}

-- | A token hiding all the core functionality required to put keys into the OS
-- data BasicKeyO = BasicKeyO
--   { _emitKey   :: KeySwitch -> IO () -- ^ How to emit a switch event to OS
--   , _repeatKey :: Keycode   -> IO () -- ^ How to signal OS to repeat a keyswitch
--   }
-- makeLenses ''KeyO

-- | A class generalizing the concept of some config that allows opening a key sink
-- class CanOpenKeyO cfg where
--   withKeyO :: forall m a. MonadUnliftIO m => cfg -> (KeyO -> m a) -> m a

-- class HasKeyO env where keyO :: Getter env KeyO

-- type CanKeyO m env = (MonadIO m, MonadReader env m, HasKeyO env)

-- | A token hiding all the functionality required to get keys from the OS
-- newtype KeyI = KeyI { _uKeyI :: IO KeySwitch }
-- makeLenses ''KeyI

-- class CanOpenKeyI cfg where
--   withKeyI :: forall m a. MonadUnliftIO m => cfg -> (KeyI -> m a) -> m a

-- class HasKeyI env where keyI :: Getter env KeyI
-- instance HasKeyI KeyI where keyI = id


-- class HasKeyGetter a where keyGetter :: Getter a KeyGetter
-- class HasKeyPutter a where keyPutter :: Getter a KeyO

  -- , _repeatCfg :: Maybe KeyRepeatCfg -- ^ Key-repeat settings

-- | The configuration options that can be passed to IOKitCfg.



-- | Runtime environment for the key-repeat process
-- data KeyRepeatEnv = KeyRepeatEnv
--   { _repeatCfg :: KeyRepeatCfg
--   , _current   :: MVar (Maybe (Async ()))
--   , _krKeyO      :: KeyO
--   }
-- makeLenses ''KeyRepeatEnv

-- instance HasKeyO KeyRepeatEnv where keyO = krKeyO
