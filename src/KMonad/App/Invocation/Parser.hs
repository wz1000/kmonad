module KMonad.App.Invocation.Parser
  ( invocationP )
where

import KMonad.Prelude

import KMonad.App.Types
import KMonad.Util.Time
import System.Keyboard hiding (switch)

import Data.Monoid (Endo(..))
import Options.Applicative
import qualified RIO.Text as T

{- NOTE: Notes to the programmer ----------------------------------------------

Dear programmer, please be aware of the following:

This module structures configurable settings into a number of categories.
- global: applicable to any kmonad invocation
- model: applicable only when we actually run a model
- input: applicable for whenever we grab a keyboard
- output: applicable whenever we simulate a keyboard

Then we specify 3 different commands KMonad can execute, and pass the correct
'categories' to them by means of a nested record:
- run: uses all 5 categories
- parse-test: uses only the parse category
- discover: uses parse and input

We don't specify values that get written into the config, but instead each
flag/argument specifies some edit or change that gets applied to the default
structure. That way default values are all specified in 'App.Types', and the
argument parser here only describes `edits` without ever having to know the full
structure.

Additionally, this makes it easier to parse global settings (e.g. log-level)
both before and after the subcommand (i.e. run/parse-test/discover) and unify
these modifications.

Additionally, this makes it easier to specify multiple flags that act on the
same settings and unify these modification. (i.e. allow --log-level for
fine-grained control, but provide -v as shorthand for --log-level debug.)

-------------------------------------------------------------------------------}

{- SECTION: setup -------------------------------------------------------------}

{- SUBSECTION: the P type -----------------------------------------------------}

-- | The type of a parser that produces an Endo (a -> a) on some type
type P a = Parser (Endo a)

-- | Run a 'P' by applying all Endo's to some default value
pRun :: Default a => P a -> Parser a
pRun = fmap (`appEndo` def)

-- | Turn a bunch of P's into a single P by applying each edit in order.
pConcat :: [P a] -> P a
pConcat ps = mconcat <$> sequenceA ps

-- | Lift a P in a to a P in s by lifting the endo using a lens
pLift :: Lens' s a -> P a -> P s
pLift l = fmap (liftEndo l)
  where liftEndo l = Endo . over l . appEndo

{- SUBSECTION: P smart constructors -------------------------------------------}

-- | Create a P that sets a lens to a value
setL :: Lens' cfg a -> Parser (Maybe a) -> P cfg
setL l p = maybe mempty (Endo . set l)  <$> p

-- | Create a P that toggles a boolean value in BasicCfg
toggleL :: Lens' cfg Bool -> Parser Bool -> P cfg
toggleL l p = bool mempty (Endo $ over l not) <$> p

-- | Create a P that on a flag triggering does some change to a cfg
onFlag :: (cfg -> cfg) -> Mod FlagFields (Endo cfg) -> P cfg
onFlag f = flag mempty (Endo $ \cfg -> f cfg)

{- SUBSECTION: shorthand ------------------------------------------------------}

-- | Make it clean to specify that an 'option' takes a Ms argument
ms :: ReadM Ms
ms = fi <$> int

-- | Make it clean to specify that an 'option' takes an Int argument
int :: ReadM Int
int = auto


{- SECTION: Parsers -----------------------------------------------------------}

{- SUBSECTION: Top-level and task parsers -------------------------------------}

-- | Parse the full 'BasicCfg' from the command-line invocation
invocationP :: Parser BasicCfg
invocationP = pRun . pConcat $ [basicCfgP, taskP]

-- | Parser that configures the tas
taskP :: P BasicCfg
taskP = hsubparser
  (  command "run"
    (info runP
     (progDesc "Run a keyboard remapping."))
  <> command "parse-test"
    (info parseTestP
     (progDesc "Check a config-file's syntax." ))
  <> command "discover"
    (info discoverP
     (progDesc "Print out what we know about key-presses." ))
  )

-- | Parser that configures and sets KMonad to 'run' a model
runP :: P BasicCfg
runP = pConcat [setL task wrapped, basicCfgP]
  where
    wrapped :: Parser (Maybe Task)
    wrapped = Just . Run <$> pRun runCfgP

    runCfgP = pConcat [ pLift modelCfg  modelCfgP
                      , pLift inputCfg  inputCfgP
                      , pLift outputCfg outputCfgP ]

-- | Parser that configures and sets KMonad to 'parse-test' a file
parseTestP :: P BasicCfg
parseTestP = pConcat [setL task (pure $ Just ParseTest), basicCfgP]

-- | Parser that configures and sets KMonad to 'discover' key input
discoverP :: P BasicCfg
discoverP = pConcat [setL task wrapped, basicCfgP]
  where
    wrapped :: Parser (Maybe Task)
    wrapped = Just . Discover <$> pRun discoverCfgP

    discoverCfgP  = pConcat [ pLift inputCfg inputCfgP, dumpKeyTableP ]

-- | A flag to the `discover` command to print out the 'KeyTable' and exit.
dumpKeyTableP :: P DiscoverCfg
dumpKeyTableP = onFlag (set dumpKeyTable True)
  (  long "dump-table"
  <> help "If provided, print the standard keytable and exit")

{- SUBSECTION: BasicCfg options -----------------------------------------------}

-- | Parse global configuration options available to every subcommand
basicCfgP :: P BasicCfg
basicCfgP = pConcat
  [ cfgFileP, logLevelP, toggleSectionsP, keyTableP, verboseP
  , cmdAllowP, cmdForbidP ]

-- | Pass a FilePath to a configuration file to load
cfgFileP :: P BasicCfg
cfgFileP = setL cfgFile wrapped
  where wrapped :: Parser (Maybe CfgFile)
        wrapped = option (Just . InRoot <$> str)
          (  long    "config"
          <> short   'f'
          <> metavar "FILE"
          <> value   Nothing
          <> help    "The kmonad configuration file to load."
          )

-- | The log-level of either error, warn, info, or debug
logLevelP :: P BasicCfg
logLevelP = setL logLevel nested
  where
    nested :: Parser (Maybe LogLevel)
    nested = option f
      (  long    "log-level"
      <> short   'l'
      <> value   Nothing
      <> help    "Logging verbosity (debug > info > warn > error)" )
      where
        f = fmap Just . maybeReader $ flip lookup
              [ ("debug", LevelDebug), ("warn", LevelWarn)
              , ("info",  LevelInfo),  ("error", LevelError) ]

-- | A flag that toggles log-section usage.
toggleSectionsP :: P BasicCfg
toggleSectionsP = toggleL logSections nested
  where
    nested :: Parser Bool
    nested = switch
      (  long "toggle-sections"
      <> help "toggle the usage of section separators in logging output")

-- | A path to a file to load the key-table from
keyTableP :: P BasicCfg
keyTableP = setL keyTableCfg nested
  where
    nested :: Parser (Maybe KeyTableCfg)
    nested = option (Just . CustomTable <$> str)
      (  long  "key-table"
      <> short 'T'
      <> value Nothing
      <> help  "Custom keyname table file to use instead of KMonad default."
      )

-- | Verbose flag, for simplicity
verboseP :: P BasicCfg
verboseP = onFlag (set logLevel LevelDebug)
  (  short 'v'
  <> help  "verbose, same as -l debug"
  )

-- | Flag that, when passed, enables shell command execution by KMonad
cmdAllowP :: P BasicCfg
cmdAllowP = onFlag (set cmdAllow True)
  (  long  "cmd-allow"
  <> help  "Enable shell-command execution in KMonad"
  )

-- | Flag that, when passed, disables shell command execution by KMonad
cmdForbidP :: P BasicCfg
cmdForbidP = onFlag (set cmdAllow False)
  (  long  "cmd-forbid"
  <> help  "Disable shell-command execution in KMonad"
  )

{- SUBSECTION: ModelCfg options -----------------------------------------------}

modelCfgP :: P ModelCfg
modelCfgP = pConcat
  [fallthroughAllowP, fallthroughForbidP, composeKeyP, macroDelayP]

-- | Flag that, when passed, disables uncaught event rethrowing.
fallthroughForbidP :: P ModelCfg
fallthroughForbidP = onFlag (set fallthrough False)
  (  long  "fallthrough-forbid"
  <> help  "Disable rethrowing uncaught events"
  )

-- | Flag that, when passed, disables uncaught event rethrowing.
fallthroughAllowP :: P ModelCfg
fallthroughAllowP = onFlag (set fallthrough True)
  (  long  "fallthrough-allow"
  <> help  "Disable rethrowing uncaught events"
  )

-- | Passable option to change compose key
composeKeyP :: P ModelCfg
composeKeyP = setL composeKey wrapped
  where wrapped :: Parser (Maybe Keyname)
        wrapped = option (Just <$> str)
          (  long  "compose"
          <> value Nothing
          <> help  "Key to use for compose-key macro-sequences."
          )

-- | Passable option to change macro-delay
macroDelayP :: P ModelCfg
macroDelayP = setL macroDelay wrapped
  where wrapped :: Parser (Maybe Ms)
        wrapped = option (Just <$> ms)
          (  long  "macro-delay"
          <> value  Nothing
          <> help  "Number of milliseconds to pause between keypresses in macros"
          )

{- SUBSECTION: InputCfg options -----------------------------------------------}

-- | Full input configuration parser
inputCfgP :: P InputCfg
inputCfgP = pConcat [ inputTokenP, startDelayP ]

{-| Parse a token-name:extra-cfg style string as an input token

Valid values:
- evdev
- evdev:/path/to/file
- llhook
- iokit
- iokit:name-pattern -}
inputTokenP :: P InputCfg
inputTokenP = setL inputToken wrapped
  where
    wrapped :: Parser (Maybe InputToken)
    wrapped = option (Just <$> maybeReader f)
      (  long  "input"
      <> short 'i'
      <> value Nothing
      <> help  h
      )
    f s = case break (== ':') s of
      ("evdev", "")    -> Just $ Evdev (EvdevCfg Nothing)
      ("evdev", ':':s) -> Just $ Evdev (EvdevCfg (Just s))
      ("llhook", "")   -> Just $ LLHook
      ("iokit", "")    -> Just $ IOKit
      _                -> Nothing
    h = mconcat
        [ "String describing how to capture the keyboard. Pattern: `name:arg` "
        , "where `name` in [evdev, llhook, iokit] and `arg` an extra input for "
        , "`evdev`, where it is the file to open, and `iokit` where it is the "
        , "keyboard name. Examples: evdev, evdev:/dev/input/event0, llhook, "
        , "iokit, iokit:my-kb" ]

-- | Configureable startup delay
startDelayP :: P InputCfg
startDelayP = setL startDelay wrapped
  where wrapped :: Parser (Maybe Int)
        wrapped = option (Just <$> int)
          (  long  "start-delay"
          <> short 's'
          <> value Nothing
          <> help  "Number of milliseconds to pause before grabbing the keyboard"
          )

{- SUBSECTION: OutputCfg options ----------------------------------------------}

-- | The full output configuration parser
outputCfgP :: P OutputCfg
outputCfgP = pConcat [ outputTokenP, keyRepeatCfgP ]

{- | Parse a token-name:extra-cfg style string as an output token

Valid values:
- uinput
- uinput:name
- sendkeys
- ext -}
outputTokenP :: P OutputCfg
outputTokenP = setL outputToken wrapped
  where
    wrapped :: Parser (Maybe OutputToken)
    wrapped = option (Just <$> maybeReader f)
      (  long "output"
      <> short 'o'
      <> value Nothing
      <> help h
      )

    f s = case break (== ':') s of
      ("uinput", "")    -> Just $ Uinput Nothing Nothing
      ("uinput", ':':s) -> Just $ Uinput (Just $ pack s) Nothing
      ("sendkeys", "")  -> Just $ SendKeys
      ("ext", "")       -> Just $ Ext
      _                 -> Nothing

    h = mconcat
        [ "String describing how to simulate the keyboard. Pattern `name:arg` "
        , "where `name` in [uinput, sendkeys, ext] and `arg` an extra input for "
        , "uinput only, where it specifies the name of the simulated keybaord. "
        , "Examples: uinput:mykb, uinput, sendkeys, ext" ]

-- | Parse a delay:rate string as a repeat configuration.
keyRepeatCfgP ::  P OutputCfg
keyRepeatCfgP = setL repeatCfg wrapped
  where
    -- Note that 'repeatCfg' itself is a 'Maybe KeyRepeatCfg', therefore we have
    -- doubly Maybe vaues. The outer Maybe indicates whether an endo should be
    -- applied, the inner maybe gets written into the config.
    wrapped :: Parser (Maybe (Maybe KeyRepeatCfg))
    wrapped = option (Just . Just <$> maybeReader f)
      (  long  "key-repeat"
      <> short 'r'
      <> value Nothing
      <> help h
      )

    -- We also have Maybe's here, but they signal whether the parse succeeded or
    -- not. If end up returning Nothing here, then the user specified a bad input
    f s = case break (== ':') s of
      (a, ':':b) -> do
        -- NOTE: Here the 'monad' is simply Maybe
        msa <- (readMaybe a :: Maybe Int)
        msb <- (readMaybe b :: Maybe Int)
        pure $ KeyRepeatCfg (fi msa) (fi msb) True
      _          -> Nothing

    h = mconcat
        [ "Pattern `delay:rate` where `delay` is the number of milliseconds "
        , "before key-repeat starts, and `rate` is the number of milliseconds "
        , "between repeat events" ]
