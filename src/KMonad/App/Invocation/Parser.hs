module KMonad.App.Invocation.Parser
  ( invocationP )
where

import KMonad.Prelude

import KMonad.App.Configurable
import KMonad.App.Types
import KMonad.Util.Name
import KMonad.Util.Time
import System.Keyboard hiding (switch, Description)

import Options.Applicative


{- SECTION: setup -------------------------------------------------------------}

{- SUBSECTION: the P type -----------------------------------------------------}

-- | The type of a parser that produces a Change on some type
type P c = Parser (Change c)

-- | Run a 'P' by applying all changes to some default value
pRun :: Default a => P a -> Parser a
pRun = fmap onDef

-- | Turn a bunch of P's into a single P by applying each edit in order.
pConcat :: [P a] -> P a
pConcat ps = mconcat <$> sequenceA ps

{- SUBSECTION: Setting transformers -------------------------------------------}

-- | Automatically create a command-line option from a 'mkOption' value
--
-- Note that in regards to the 'Mod' fields we already:
-- * set the value to Nothing (important for our mechanism)
-- * set the long name to the Option's name
-- * set the help to the Options's description
--
-- Other mods can still be passed, namely 'short' and 'metavar'
fromOption :: Option c a -> ReadM a -> Mod OptionFields (Maybe a) -> P c
fromOption o r m = maybe mempty (runOption o) <$> p
  where p = option (Just <$> r)
          (  m
          <> value Nothing
          <> long (unpack $ o^.name)
          <> help (unpack $ o^.description))

-- | Automatically create a command-line flag from a 'mkFlag' value
--
-- Note that in regards to the 'Mod' fields we already:
-- * set the long name to the Flag's name
-- * set the help to the Flag's description
fromFlag :: Flag c -> Mod FlagFields (Change c) -> P c
fromFlag f m = flag mempty (runFlag f)
  (  m
  <> long (unpack $ f^.name)
  <> help (unpack $ f^.description))

{- SUBSECTION: input parsers ---------------------------------------------------
In addition to the builtin `str` we specify a few more shorthands to specify how
to read an argument.
-------------------------------------------------------------------------------}

-- | Read a millisecond value
ms_ :: ReadM Ms
ms_ = fi <$> int
{- NOTE: `_` disambiguates from 'ms', which is an Iso between Int and Ms -}

-- | Read an int
int :: ReadM Int
int = auto

-- | Read a txt value
txt :: ReadM Text
txt = pack <$> str

-- | Wrap a reader in a Just
j :: ReadM a -> ReadM (Maybe a)
j = fmap Just

{- SECTION: Parsers -----------------------------------------------------------}

{- SUBSECTION: Top-level and task parsers -------------------------------------}

-- | Parse the top-level change to the default BasicCfg passed on the cmd-line
invocationP :: P BasicCfg
invocationP = pConcat [ basicCfgP, taskP ]

-- | Parse the task
--
-- Note that, in addition to a tasks config-parser, we also run 'basicCfgP'
-- again, this is to ensure that basic configurations can be passed both before
-- and after the subcommand.
taskP :: P BasicCfg
taskP = let f n p d = (command n (info (pConcat [p, basicCfgP]) (progDesc d))) in
  hsubparser $ mconcat
    [ f "run"        runP       "Run a keyboard remapping."
    , f "parse-test" parseTestP "Check a config-file for errors"
    , f "discover"   discoverP  "Interactively explore button keycodes and names"
    ]

-- | Helper function to generate task-insertion changes
setTask :: Description -> (a -> Task) -> (Parser a) -> P BasicCfg
setTask d f p = (\t -> mkChange d (\c -> c & task .~ t)) <$> (f <$> p)

-- | Parse the @run@ command ad ints configuration
--
-- This pulls in all configurable settings for: model, input, and output
runP :: P BasicCfg
runP = setTask "task:run" Run runCfgP
  where runCfgP = pRun . pConcat $
          [ modelCfgP
          , inputCfgP
          , parseCfgP
          , outputCfgP
          ]

-- | Parse the @parse-test@ command and its configuration
--
-- This pull in no extra configuration
parseTestP :: P BasicCfg
parseTestP = setTask "task:parse-test" (const ParseTest) (pure ())

-- | Parse the @discover@ command and its configuration
--
-- This pulls in configurable settings for input, and some discover-specific
-- config.
discoverP :: P BasicCfg
discoverP = setTask "task:discover" Discover discoverCfgP
  where discoverCfgP = pRun . pConcat $
          [ inputCfgP
          , parseCfgP
          , fromFlag flagDumpKeyTable mempty
          , fromFlag flagInescapable  mempty
          ]

{- SUBSECTION: Parsers by category --------------------------------------------}

-- | All settings that apply, regardless of task
basicCfgP :: P BasicCfg
basicCfgP = pConcat
  [ -- Options
    fromOption optCfgFile  (j str) (short 'f' <> metavar "FILE")
  , fromOption optLogLevel logLvl  (short 'l')
  , fromOption optKeyTable tblStr  (short 'k')
    -- Flags
  , fromFlag flagVerbose     (short 'v')
  , fromFlag flagSectionsOff mempty
  , fromFlag flagSectionsOn  mempty
  , fromFlag flagCommandsOn  mempty
  , fromFlag flagCommandsOff (short 's')
  ]
  where
    tblStr = CustomTable <$> str
    logLvl = maybeReader $ flip lookup
        [ ("debug", LevelDebug), ("warn", LevelWarn)
        , ("info",  LevelInfo),  ("error", LevelError) ]

-- | All settings that apply to parsing the config file
parseCfgP :: HasParseCfg c => P c
parseCfgP = pConcat
  [ fromOption optComposeKey txt (metavar "KEYNAME")
  ]

-- | All settings that apply to running a keyboard remapping
modelCfgP :: HasModelCfg c => P c
modelCfgP = pConcat
  [ -- Options
    fromOption optMacroDelay ms_ mempty
    -- Flags
  , fromFlag flagFallthroughOff mempty
  , fromFlag flagFallthroughOn  mempty
  ]

-- | All settings that apply to capturing keyboard input
inputCfgP :: HasInputCfg c => P c
inputCfgP = pConcat
  [ fromOption optEvdevDeviceFile (j str) mempty
  , fromOption optIOKitDeviceName (j txt) mempty
  , fromOption optStartDelay      ms_     (short 'd')
  ]

-- | All settings that apply to generating keyboard output
outputCfgP :: HasOutputCfg c => P c
outputCfgP = pConcat
  [ fromOption optUinputDeviceName (j txt) mempty
  , fromOption optRepeatRate       ms_     mempty
  , fromOption optRepeatDelay      ms_     mempty
  ]
