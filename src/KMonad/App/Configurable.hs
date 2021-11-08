{-# LANGUAGE ExistentialQuantification #-}
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

NOTE:
All changes and options are 1-ops. I.e. -> flag (flag thing) == flag thing

-------------------------------------------------------------------------------}

module KMonad.App.Configurable where

import Preface

import Data.Monoid

import KMonad.App.Cmds
import KMonad.App.Locale
import KMonad.App.Logging
import KMonad.App.Types

import KMonad.Util.Name
import KMonad.Util.Time

import Options.Applicative (ReadM, eitherReader)
import System.Keyboard hiding (Name, Description)

{- SECTION: Types -------------------------------------------------------------}

-- FIXME: this shouldn't live here
type Description = Text
class HasDescription a where description :: Lens' a Description

-- | An iso between a monoidal foldable thing and that thing wrapped in a Just
-- if it isn't empty, or Nothing if it is.
--
-- This makes it notationally convenient to specify text arguments that get
-- inserted into 'Maybe Text'. If an empty string is passed, we set it to
-- Nothing, otherwise we set it to 'Just' that text.
-- _NonEmpty :: (Foldable f, Monoid (f a)) => Iso' (f a) (Maybe (f a))
-- _NonEmpty = iso
--   (\a -> bool (Just a) Nothing (null a))
--   (\ma -> fromMaybe mempty ma)
--
-- NOTE: Found better solution

{- SUBSECTION: Basic types ----------------------------------------------------}

type FlagName   = Text -- ^ A name that refers to a flag
type OptionName = Text -- ^ A name that refers to an option

{- SUBSECTION: Errors ---------------------------------------------------------}

-- | Things that can go wrong with configurables
data ConfigurableException
  = UnknownConfigurable  Name
  -- ^ Encountered a refence to an unknown configurable
  | BadConfigurableValue Name Text Name
  -- ^ Encountered a bad value for a known configurable
  deriving (Eq, Show)

instance Exception ConfigurableException where
  displayException (UnknownConfigurable n) = unpack . mconcat $
    ["Encountered unknown setting: " <> n]
  displayException (BadConfigurableValue n t m) = unpack . mconcat $
    ["Encountered unreadable value for '", n, "': ", t
    , ". Expecting a ", m, " value." ]

{- SUBSECTION: Change ---------------------------------------------------------}

-- | A datatype representing some change to a structure
data Change s = Change
  { _cDescrip :: Description
  , _endo     :: Endo s
  }
makeClassy ''Change

instance HasDescription (Change s) where description = cDescrip

-- | A list of changes, we make heavy use of the monoid properties
type Changes s = [Change s]

-- | Create a new 'Changes' value
mkChange :: Text -> (s -> s) -> Changes s
mkChange t = singleton . Change t . Endo

-- | Create a 'Change' that sets some property to some value
setProp :: Text -> Traversal' s a -> a -> Changes s
setProp t l a = mkChange t (\s -> s & l .~ a)

-- | Lift an endo into an endo over some larger structure using a traversal
liftEndo :: Traversal' s a -> Endo a -> Endo s
liftEndo l (Endo f) = (Endo $ over l f)

-- | Lift a change into some larger structure with a traversal describing the embedding
liftChange :: Traversal' s a -> Change a -> Change s
liftChange l (Change d e) = Change d (liftEndo l e)

-- | Any change to the root 'RootCfg' structure
type CfgChange = Changes RootCfg

{- SUBSECTION: ArgParse --------------------------------------------------------
I'd like to use ReadM from optparse-applicative directly, but I can't find a way
to just run a ReadM on some text. So instead we just write a very small, simple
reader that can be exported to ReadM for the optparse stuff, and can be used in
our config parser directly. That way we don't write the parsers twice.
-------------------------------------------------------------------------------}

-- | A wrapper around a ReaderT that turns text into maybe value
newtype ArgParse a = ArgParse { _uArgParse :: ReaderT Text Maybe a }
  deriving (Functor, Applicative, Monad, MonadReader Text)
makeClassy ''ArgParse

-- | Run a 'ArgParse' on some text and return the arg or fail
runArgParse :: HasArgParse s a => s -> Text -> Maybe a
runArgParse p = runReaderT (p^.argParse.uArgParse)

-- | An ArgParse that uses a types 'Read' instance
readParse :: Read a => ArgParse a
readParse = ArgParse . ReaderT $ readMaybe . unpack

-- | An ArgParse that wraps a simple parsing function
parseWith :: (Text -> Maybe a) -> ArgParse a
parseWith = ArgParse . ReaderT

-- | An ArgParse that matches names to values
parseMatch :: [(Text, a)] -> ArgParse a
parseMatch = parseWith . flip lookup

-- | An 'ArgType' value, pairing some parser with a name
data Arg a = Arg
  { _argName  :: Name
  , _aArgParse :: ArgParse a
  }
makeClassy ''Arg

instance HasName     (Arg a)   where name     = argName
instance HasArgParse (Arg a) a where argParse = aArgParse

instance Functor Arg where
  fmap f a = Arg (a^.argName) (fmap f $ a^.argParse)

{- SUBSECTION: Flag -----------------------------------------------------------}

-- | A datatype representing some fixed change to a structure
data Flag s = Flag
  { _fName        :: FlagName
  , _fDescription :: Text
  , _fChange      :: Changes s}
makeLenses ''Flag

instance HasName (Flag s) where name = fName
instance HasDescription (Flag s) where description = fDescription

-- | Create a flag that sets some traversal to some fixed value when triggered
mkFlag :: FlagName -> Traversal' s a -> a -> Text -> Flag s
mkFlag n l a d = Flag n d $ setProp ("flag:" <> n) l a

-- | Extract the change from a flag
runFlag :: Flag s -> Changes s
runFlag = _fChange

-- | Existential wrapper around a flag specified on some existing cfg and an
-- embedding of that cfg into a RootCfg
data AnyFlag = forall s. AnyFlag
  { fEmbed :: Traversal' RootCfg s
  , fFlag  :: Flag s
  }
-- NOTE: this doesn't work here: makeLenses ''AnyFlag. Because existential?

{- NOTE: These have to be done manually too, I can never 'get at' the flag in an
 AnyFlag, because that would leak a wrapped type. However, I can always get a
 name and description. -}
instance HasName AnyFlag where
  name = lens
    (\(AnyFlag _ f)   -> f^.name)
    (\(AnyFlag e f) n -> AnyFlag e $ f & name .~ n)

instance HasDescription AnyFlag where
  description = lens
    (\(AnyFlag _ f)   -> f^.description)
    (\(AnyFlag e f) n -> AnyFlag e $ f & description .~ n)

-- | How to apply 'AnyFlag' to a 'RootCfg'
runAnyFlag :: AnyFlag -> Changes RootCfg
runAnyFlag (AnyFlag l f) = map (liftChange l) $ runFlag f

{- SUBSECTION: Option ---------------------------------------------------------}

-- | A datatype representing some settable option on a structure
data Option s = forall a. Option
  { _oName        :: OptionName
  , _oDescription :: Text
  , _oArg         :: Arg a
  , _oChange      :: a -> Changes s }
makeLenses ''Option

instance HasName        (Option s) where name        = oName
instance HasDescription (Option s) where description = oDescription

-- | Create an option that sets some traversal to some passed value when called
mkOption :: Show a
  => OptionName -> Traversal' s a -> Arg a -> Text -> Option s
mkOption n l a d = Option n d a
  (\x -> mkChange ("option:" <> n <> ":" <> tshow x) $ set l x)

-- | Existential wrapper around an option on some configuration and an embedding
-- of that configuration into a RootCfg
data AnyOption = forall s. AnyOption
  { oEmbed  :: Traversal' RootCfg s
  , oOption :: Option s}

instance HasName AnyOption where
  name = lens
    (\(AnyOption _ o)   -> o^.name)
    (\(AnyOption e o) n -> AnyOption e $ o & name .~ n)

instance HasDescription AnyOption where
  description = lens
    (\(AnyOption _ o)   -> o^.description)
    (\(AnyOption e o) n -> AnyOption e $ o & description .~ n)

-- | Apply an option-change to a RootCfg
runAnyOption :: AnyOption -> Text -> Either ConfigurableException CfgChange
runAnyOption (AnyOption l o) t = map (liftChange l) <$> readOption o t

{- SECTION: Operations --------------------------------------------------------}

{- SUBSECTION: ArgParse -------------------------------------------------------}

-- | View anything that has an 'Arg' as an optparse-applicate 'ReadM'
readM :: HasArg s a => Getter s (ReadM a)
readM = to $ eitherReader . f
  where
    f a s = case runArgParse (a^.arg.argParse) . pack $ s of
      Nothing -> Left . unpack $ "Could not parse: " <> a^.argName
      Just a  -> Right a

{- SUBSECTION: Change operations ----------------------------------------------}

-- | Apply an option by passing it an arg
-- runOption :: Option s a -> a -> Change s
-- runOption = _oChange

-- | Try to apply an option by parsing its arg from some text
readOption :: Option s -> Text -> Either ConfigurableException (Changes s)
readOption (Option n _ a c) t = maybe e (Right . c) . runArgParse a $ t
  where e = Left $ BadConfigurableValue n t (a^.argName)

-- | Apply a change to some structure
appChange :: Changes s -> s -> s
appChange = appEndo . mconcat . toListOf (folded.endo)

-- | Apply a change to some default value
onDef :: Default s => Changes s -> s
onDef = (`appChange` def)

{- SECTION: Values ------------------------------------------------------------}

{- SUBSECTION: ArgParse -------------------------------------------------------}

-- | An argument that accepts pure text
txtArg :: Arg Text
txtArg = Arg "raw text" $ ask

-- | An argument that accepts pure text as a filepath
--
-- TODO: Could add validation here
fileArg :: Arg FilePath
fileArg = unpack <$> txtArg

-- | An argument that accepts integers
intArg :: Arg Int
intArg = Arg "integer" $ readParse

-- | An argument that accepts integers as milliseconds
msArg :: Arg Ms
msArg = fi <$> intArg

-- | An argument that accepts a LogLevel string
logLevelArg :: Arg LogLevel
logLevelArg = Arg "log level" . parseMatch $
  [ ("error" , LevelError)
  , ("warn"  , LevelWarn)
  , ("info"  , LevelInfo)
  , ("debug" , LevelDebug) ]

-- | An argument that accepts a LogColor string
logColorArg :: Arg LogColor
logColorArg = Arg "log color" . parseMatch $
  [ ("dark-bg" , DarkBG)
  , ("light-bg", LightBG)
  , ("none"    , Monochrome) ]

-- | Parse 'on' as True and 'off' as False
onOffArg :: Arg Bool
onOffArg = Arg "on or off" . parseMatch $
  [("on", True) , ("off", False)]

-- | Parse text as a simplecmd
--
-- Note that we don't yet support any of the delay or forking settings via the
-- CLI. Those are currently only accessible via the config-file.
cmdArg :: Arg Cmd
cmdArg = simpleCmd <$> txtArg

{- SUBSECTION: Overview -------------------------------------------------------}

-- | All flags
allFlags :: Named AnyFlag
allFlags = mconcat
  [ rootFlags, logFlags, localeFlags, inputFlags, cmdsFlags
  , outputFlags, modelFlags, discoverFlags]

-- | All options
allOptions :: Named AnyOption
allOptions = mconcat
  [ rootOptions, logOptions, localeOptions, inputOptions, cmdsOptions
  , outputOptions, modelOptions, discoverOptions]

-- | All flags that apply regardless of task
generalFlags :: Named AnyFlag
generalFlags = mconcat
  [ rootFlags, logFlags, cmdsFlags ]

-- | All flags that apply regardless of task
generalOptions :: Named AnyOption
generalOptions = mconcat
  [ rootOptions, logOptions, cmdsOptions ]

{- SUBSECTION: RootCfg -------------------------------------------------------}

rootFlags :: Named AnyFlag
rootFlags = byName . map (AnyFlag id) $
  [
  ]

rootOptions :: Named AnyOption
rootOptions = byName . map (AnyOption id) $
  [
    mkOption "cfg-file" cfgFile (Just <$> fileArg)
      "File containing kmonad's keymap configuration"
  ]

{- SUBSECTION: Logging --------------------------------------------------------}

logFlags :: Named AnyFlag
logFlags = byName . map (AnyFlag logCfg) $
  [
    mkFlag "verbose" logLevel LevelDebug
      "Make KMonad very verbose: same as log-level debug"
  ]

logOptions :: Named AnyOption
logOptions = byName . map (AnyOption logCfg) $
  [
    mkOption "log-level" logLevel logLevelArg
      "Logging verbosity: 'debug' > 'info' > 'warn' > 'error'"

  , mkOption "log-sections" useSep onOffArg
      "Whether to use section separators in the logging output"

  , mkOption "log-color" logColor logColorArg
      "What kind of coloring to use, for 'dark-bg', 'light-bg', or 'none'"
  ]

{- SUBSECTION: Locale ---------------------------------------------------------}

localeFlags :: Named AnyFlag
localeFlags = byName []

localeOptions :: Named AnyOption
localeOptions = byName . map (AnyOption taskLocaleCfg) $
  [
    mkOption "key-table" keyTableCfg (CustomTable <$> fileArg)
      "File from which to load a tabular keytable"

  , mkOption "compose-key" composeKey txtArg
      "Keyname for key to use as compose key in symbol-macros"

  , mkOption "std-shift" stdShift txtArg
      "Keyname for key to use as shift in shifted-macros"
  ]

{- SUBSECTION: Cmds -----------------------------------------------------------}

cmdsFlags :: Named AnyFlag
cmdsFlags  = byName . map (AnyFlag cmdsCfg) $
  [
    mkFlag "safe" enabled False
      "Run KMonad in 'safe' mode by disabling all shell-commands, same as 'commands off'"
  ]

cmdsOptions :: Named AnyOption
cmdsOptions = byName . map (AnyOption cmdsCfg) $
  [
    mkOption "commands" enabled onOffArg
      "Whether KMonad is able to execute shell-commands"

  , mkOption "cmd-on-start" (hooks.at OnStart) (Just <$> cmdArg)
      "Command to execute right after startup"

  , mkOption "cmd-pre-acquire" (hooks.at PreAcquire) (Just <$> cmdArg)
      "Command to execute right before acquiring key IO"

  , mkOption "cmd-post-acquire" (hooks.at PostAcquire) (Just <$> cmdArg)
      "Command to execute right after acquiring key IO"

  , mkOption "cmd-pre-release" (hooks.at PreRelease) (Just <$> cmdArg)
      "Command to execute right before releasing key IO"

  , mkOption "cmd-post-release" (hooks.at PostRelease) (Just <$> cmdArg)
      "Command to execute right after releasing key IO"

  , mkOption "cmd-on-exit" (hooks.at OnExit) (Just <$> cmdArg)
      "Command to execute right before shutdown"
  ]

{- SUBSECTION: ModelCfg -------------------------------------------------------}

modelFlags :: Named AnyFlag
modelFlags = byName . map (AnyFlag taskModelCfg) $
  [
    mkFlag "fallthrough-off" fallthrough False
      "Disable uncaught events being retransmitted to the OS"

  , mkFlag "fallthrough-on" fallthrough True
      "Enable uncaught events being retransmitted to the OS"
  ]

modelOptions :: Named AnyOption
modelOptions = byName . map (AnyOption taskModelCfg) $
  [
    mkOption "macro-delay" macroDelay msArg
      "Time (ms) between taps when sending keyboard macros to the OS"

  , mkOption "fallthrough" fallthrough onOffArg
      "Whether to retransmit uncaught events"
  ]

{- SUBSECTION: InputCfg -------------------------------------------------------}

inputFlags :: Named AnyFlag
inputFlags = byName []

inputOptions :: Named AnyOption
inputOptions = byName . map (AnyOption taskInputCfg) $
  [
    mkOption "evdev-device-file"
      (inputToken._Evdev.evdevPath) (Just <$> fileArg)
      "Set the path to the evdev device file (only does something on linux)"

  , mkOption "iokit-device-name"
      (inputToken._IOKit.productStr) (Just <$> txtArg)
      "Set the name used to select the IOKit input keyboard (only does something on Mac)"

  , mkOption "start-delay" (startDelay . from ms) msArg
      "Set the time that we wait before we grab the keyboard (to release enter)"
  ]

-- {- SUBSECTION: OutputCfg ------------------------------------------------------}

outputFlags :: Named AnyFlag
outputFlags = byName []

outputOptions :: Named AnyOption
outputOptions = byName . map (AnyOption taskOutputCfg) $
  [
    mkOption "uinput-device-name"
      (outputCfg.outputToken.outputName) (Just <$> txtArg)
      "Set the name of the generated keyboard in Linux"

  , mkOption "repeat-rate" (outputCfg.repeatRate.from ms) msArg
      "Set the time between key-repeat events generated by KMonad"

  , mkOption "repeat-delay" (outputCfg.repeatDelay.from ms) msArg
      "Set the time before KMonad starts generating key-repeat events"
  ]

{- SUBSECTION: Task-specific settings -----------------------------------------}

discoverFlags :: Named AnyFlag
discoverFlags = byName . map (AnyFlag (task._Discover)) $
  [
    mkFlag "dump-keytable" dumpKeyTable True
      "Instruct `discover` to dump its keytable to stdout and exit"

  , mkFlag "inescapable" escapeExits False
      "Do not exit `discover` on pressing escape"

  , mkFlag "check-config" checkConfig True
      "If passed, also load the config-file to check for options"
  ]

discoverOptions :: Named AnyOption
discoverOptions = byName []
