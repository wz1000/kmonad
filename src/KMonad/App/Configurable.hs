{- EXPLANATION: Configuration concept ------------------------------------------

KMonad gets its configuration from 2 different sources. There is the invocation
passed on the command-line, and there is the configuration that gets parsed from
config-file. Originally, both of these parsers functioned nearly independently
(Invocation imported a few parsing functions), and they both functioned by
defining a language to address the concrete configuration record defined by the
app.

This has been changed. Instead of encoding a language that that gets parsed into
configuration-record changes, we define a collection of changes. These changes
can be 'abstract' and are independent from the concrete configuration record
specification. Instead, they interface with this record through a collection of
lenses.

...

I want to explain more, but it seems too wordy. Just gonna program for a bit.

-------------------------------------------------------------------------------}

module KMonad.App.Configurable where

import KMonad.Prelude

import Data.Monoid

import KMonad.App.Types
import KMonad.Util.Logging
import KMonad.Util.Name
import KMonad.Util.Time

import System.Keyboard hiding (Name, Description)

{- SUBSECTION: Types ----------------------------------------------------------}

type Description = Text

class HasDescription a where description :: Lens' a Description

-- | A datatype representing some change to a structure
data Change s = Change
  { _changes :: [Text] -- ^ Description of the changes applied
  , _endo    :: Endo s -- ^ Function to apply the change
  }
makeClassy ''Change

instance Semigroup (Change s) where
  a <> b = Change (a^.changes <> b^.changes) (a^.endo    <> b^.endo)

instance Monoid (Change s) where
  mempty = Change [] mempty

-- | Create a new 'Change' value
mkChange :: Text -> (s -> s) -> Change s
mkChange c = Change [c] . Endo

-- | A datatype representing some fixed change to a structure
data Flag s = Flag
  { _fName        :: Name
  , _fDescription :: Text
  , _fChange      :: Change s}
makeLenses ''Flag

instance HasName (Flag s) where name = fName
instance HasDescription (Flag s) where description = fDescription

-- | Create a flag that sets some traversal to some fixed value when triggered
mkFlag :: Name -> Traversal' s a -> a -> Text -> Flag s
mkFlag n l a d = Flag n d
  (mkChange ("flag:" <> n) $ set l a)

-- | A datatype representing some settable option on a structure
data Option s a = Option
  { _oName        :: Name
  , _oDescription :: Text
  , _oChange      :: a -> Change s }
makeLenses ''Option

instance HasName (Option s a) where name = oName
instance HasDescription (Option s a) where description = oDescription

-- | Create an option that sets some traversal to some passed value when called
mkOption :: Show a => Text -> Traversal' s a -> Text -> Option s a
mkOption n l d = Option n d
  (\a -> mkChange ("option:" <> n <> ":" <> tshow a) $ set l a)

{- SUBSECTION: Operations -----------------------------------------------------}

-- | Extract the change from an option
runOption :: Option s a -> a -> Change s
runOption = _oChange

-- | Extract the change from a flag
runFlag :: Flag s -> Change s
runFlag = _fChange

-- | Apply a change to some structure
appChange :: Change s -> s -> s
appChange = appEndo . view endo

-- | Apply all changes to some default value
onDef :: Default s => Change s -> s
onDef = (`appChange` def)

-- | Lift a change into some larger structure with a lens describing the embedding
liftChange :: Lens' s a -> Change a -> Change s
liftChange l (Change cs (Endo f)) = Change cs (Endo $ over l f)

{- SUBSECTION: Util -----------------------------------------------------------}

-- selecting :: [(Name, a)] ->


{- SECTION: Values ------------------------------------------------------------}

{- SUBSECTION: BasicCfg -------------------------------------------------------}

-- | Option that sets the log level
optLogLevel :: HasBasicCfg c => Option c LogLevel
optLogLevel = mkOption "log-level" logLevel
  "Logging verbosity: debug > info > warn > error"

-- | Option that sets the config file
optCfgFile :: HasBasicCfg c => Option c (Maybe FilePath)
optCfgFile = mkOption "cfg-file" cfgFile
  "File containing kmonad's keymap configuration"

-- | Option that sets the KeyTable file
optKeyTable :: HasBasicCfg c => Option c KeyTableCfg
optKeyTable = mkOption "key-table" keyTableCfg
  "File containing kmonad's keytable configuration"

-- | Flag that sets the log level to debug
flagVerbose :: HasBasicCfg c => Flag c
flagVerbose = mkFlag "verbose" logLevel LevelDebug
  "Make KMonad very verbose: same as log-level debug"

-- | Flag that sets logging-sections to off
flagSectionsOff :: HasBasicCfg c => Flag c
flagSectionsOff = mkFlag "sections-off" logSections False
  "Disable printing section separators while logging"

-- | Flag that sets logging sections to on
flagSectionsOn :: HasBasicCfg c => Flag c
flagSectionsOn = mkFlag "sections-on" logSections True
  "Enable printing section separators while logging"

-- | Flag that sets external command execution to on
flagCommandsOn :: HasBasicCfg c => Flag c
flagCommandsOn = mkFlag "commands-on" cmdAllow True
  "Enable the execution of external commands"

-- | Flag that sets external command execution to off
flagCommandsOff :: HasBasicCfg c => Flag c
flagCommandsOff = mkFlag "commands-off" cmdAllow False
  "Disable the execution of external commands (safe-mode)"

{- SUBSECTION: ModelCfg -------------------------------------------------------}

-- | Option that sets the compose-key
optComposeKey :: HasModelCfg c => Option c Keyname
optComposeKey = mkOption "compose-key" composeKey
  "The key used to signal the beginning of a compose-sequence to the OS"

-- | Option that sets the time between taps in key-macros
optMacroDelay :: HasModelCfg c => Option c Ms
optMacroDelay = mkOption "macro-delay" macroDelay
  "Time (ms) between taps when sending keyboard macros to the OS"

-- | Flag that sets fallthrough to off
flagFallthroughOff :: HasModelCfg c => Flag c
flagFallthroughOff = mkFlag "fallthrough-off" fallthrough False
  "Disable uncaught events being retransmitted to the OS"

-- | Flag that sets fallthrough to on
flagFallthroughOn :: HasModelCfg c => Flag c
flagFallthroughOn = mkFlag "fallthrough-on" fallthrough True
  "Enable uncaught events being retransmitted to the OS"


{- SUBSECTION: InputCfg -------------------------------------------------------}

-- | Option that specifies which evdev file to load when on Linux
optEvdevDeviceFile :: HasInputCfg c => Option c (Maybe FilePath)
optEvdevDeviceFile = mkOption "evdev-device-file" (inputToken._Evdev.evdevPath)
  "Set the path to the evdev device file (only does something on linux)"

-- | Option that specifies which IOKit keyboard name to use when on Mac
optIOKitDeviceName :: HasInputCfg c => Option c (Maybe Text)
optIOKitDeviceName = mkOption "iokit-device-name" (inputToken._IOKit.productStr)
  "Set the name used to select the IOKit input keyboard (only does something on Mac)"

-- | Option that sets the startup delay
optStartDelay :: HasInputCfg c => Option c Ms
optStartDelay = mkOption "start-delay" (startDelay . from ms)
  "Set the time that we wait before we grab the keyboard (to release enter)"

{- SUBSECTION: OutputCfg ------------------------------------------------------}

-- | Option that sets the output name
--
-- Note that this only does something on Linux, where it sets the name on the
-- uinput device.
optUinputDeviceName :: HasOutputCfg c => Option c (Maybe Text)
optUinputDeviceName = mkOption "output-name" (outputCfg.outputToken.outputName)
  "Set the name of the generated keyboard in Linux"

-- | Option that sets the key repeat rate
optRepeatRate :: HasOutputCfg c => Option c Ms
optRepeatRate = mkOption "repeat-rate" (outputCfg.repeatRate.from ms)
  "Set the time between key-repeat events generated by KMonad"

-- | Option that sets the key repeat delay
optRepeatDelay :: HasOutputCfg c => Option c Ms
optRepeatDelay = mkOption "repeat-delay" (outputCfg.repeatDelay.from ms)
  "Set the time before KMonad starts generating key-repeat events"

{- SUBSECTION: Task-specific settings -----------------------------------------}

flagDumpKeyTable :: HasDiscoverCfg c => Flag c
flagDumpKeyTable = mkFlag "dump-keytable" dumpKeyTable True
  "Instruct `discover` to dump its keytable to stdout and exit"

flagInescapable :: HasDiscoverCfg c => Flag c
flagInescapable = mkFlag "inescapable" escapeExits False
  "Do not exit `discover` on pressing escape"
