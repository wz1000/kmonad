
module KMonad.App.Cfg.Types where

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

-- | Create a flag that sets some lens to some fixed value when triggered
mkFlag :: Name -> Lens' s a -> a -> Text -> Flag s
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

-- | Create an option that sets some lens to some passed value when called
mkOption :: Show a => Text -> Lens' s a -> Text -> Option s a
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

-- | Lift a change into some larger structure with a lens describing the embedding
liftChange :: Lens' s a -> Change a -> Change s
liftChange l (Change cs (Endo f)) = Change cs (Endo $ over l f)

{- SECTION: Values ------------------------------------------------------------}

{- SUBSECTION: BasicCfg -------------------------------------------------------}

-- | Option that sets the log level
optLogLevel :: Option BasicCfg LogLevel
optLogLevel = mkOption "log-level" logLevel
  "Logging verbosity: debug > info > warn > error"

-- | Option that sets the config file
optCfgFile :: Option BasicCfg (Maybe FilePath)
optCfgFile = mkOption "cfg-file" cfgFile
  "File containing kmonad's keymap configuration"

-- | Option that sets the KeyTable file
optKeyTable :: Option BasicCfg KeyTableCfg
optKeyTable = mkOption "key-table" keyTableCfg
  "File containing kmonad's keytable configuration"

-- | Flag that sets the log level to debug
flagVerbose :: Flag BasicCfg
flagVerbose = mkFlag "verbose" logLevel LevelDebug
  "Make KMonad very verbose: same as log-level debug"

-- | Flag that sets logging-sections to off
flagSectionsOff :: Flag BasicCfg
flagSectionsOff = mkFlag "sections-off" logSections False
  "Disable printing section separators while logging"

-- | Flag that sets logging sections to on
flagSectionsOn :: Flag BasicCfg
flagSectionsOn = mkFlag "sections-on" logSections True
  "Enable printing section separators while logging"

-- | Flag that sets external command execution to on
flagCommandsOn :: Flag BasicCfg
flagCommandsOn = mkFlag "commands-on" cmdAllow True
  "Enable the execution of external commands"

-- | Flag that sets external command execution to off
flagCommandsOff :: Flag BasicCfg
flagCommandsOff = mkFlag "commands-off" cmdAllow False
  "Disable the execution of external commands"

{- SUBSECTION: ModelCfg -------------------------------------------------------}

-- | Option that sets the compose-key
optComposeKey :: Option ModelCfg Keyname
optComposeKey = mkOption "compose-key" composeKey
  "The key used to signal the beginning of a compose-sequence to the OS"

-- | Option that sets the time between taps in key-macros
optMacroDelay :: Option ModelCfg Ms
optMacroDelay = mkOption "macro-delay" macroDelay
  "Time (ms) between taps when sending keyboard macros to the OS"

-- | Flag that sets fallthrough to off
flagFallthroughOff :: Flag ModelCfg
flagFallthroughOff = mkFlag "fallthrough-off" fallthrough False
  "Disable uncaught events being retransmitted to the OS"

-- | Flag that sets fallthrough to on
flagFallthroughOn :: Flag ModelCfg
flagFallthroughOn = mkFlag "fallthrough-on" fallthrough True
  "Enable uncaught events being retransmitted to the OS"


{- SUBSECTION: InputCfg -------------------------------------------------------}

-- CONTINUE HERE


-- cfgFile :: Maybe FilePath -> Setting BasicCfg
-- cfgFile p = Setting $ Endo



-- cfgFileP = setL cfgFile wrapped
--   where wrapped :: Parser (Maybe (Maybe FilePath))
--         wrapped = option (Just . Just <$> str)
--           (  long    "config"
--           <> short   'f'
--           <> metavar "FILE"
--           <> value   Nothing
--           <> help    "The kmonad configuration file to load."
--           )
