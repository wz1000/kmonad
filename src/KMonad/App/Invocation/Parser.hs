module KMonad.App.Invocation.Parser
  ( invocationP )
where

import KMonad.Prelude

import KMonad.App.Types
import KMonad.Util.Time
import System.Keyboard hiding (switch)

import Options.Applicative
import qualified RIO.Text as T

{- NOTE: Notes to the programmer ----------------------------------------------

Dear programmer, please be aware of the following:

This module structures configurable settings into a number of categories.
- global: applicable to any kmonad invocation
- model: applicable only when we actually run a model
- input: applicable for whenever we grab a keyboard
- output: applicable whenever we simulate a keyboard
- parse: applicable whenever we parse a config file

Then we specify 3 different commands KMonad can execute, and pass the correct
'categories' to them by means of a nested record:
- run: uses all 5 categories
- parse-test: uses only the parse category
- discover: uses parse and input

-------------------------------------------------------------------------------}

{- NOTE: top level parser -----------------------------------------------------}

-- | Top level parser
invocationP :: Parser Invocation
invocationP = Invocation
  <$> globalCfgP
  <*> taskP

-- | Parse a Task
taskP :: Parser Task
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

-- | Collects all the passable options for a run into a single parser
runP :: Parser Task
runP = let cfg = RunCfg <$> modelCfgP
                        <*> inputCfgP
                        <*> outputCfgP
                        <*> parseCfgP
       in Run <$> cfg

-- | Collects all the passable options for a parse-test into a single parser
parseTestP :: Parser Task
parseTestP = ParseTest <$> parseCfgP

-- | Collects all the passable options for a discover run into a single parser
discoverP :: Parser Task
discoverP = let cfg = DiscoverCfg <$> inputCfgP
                                  <*> parseCfgP
                                  <*> dumpEnUSP
            in Discover <$> cfg

{- NOTE: global configuration options -----------------------------------------}

-- | Parse global configuration options available to every subcommand
globalCfgP :: Parser GlobalCfg
globalCfgP = GlobalCfg <$> logLevelP <*> disableSectionsP <*> keyTableP

-- | The log-level of either error, warn, info, or debug
logLevelP :: Parser LogLevel
logLevelP = option f
  (  long    "log-level"
  <> short   'l'
  <> value   LevelWarn
  <> help    "Logging verbosity (debug > info > warn > error)" )
  where
    f = maybeReader $ flip lookup
          [ ("debug", LevelDebug), ("warn", LevelWarn)
          , ("info",  LevelInfo),  ("error", LevelError) ]

-- | A flag that, when passed, disables section separators in logging
disableSectionsP :: Parser Bool
disableSectionsP = flag True False
  (  long "no-log-sections"
  <> help "Disable section separators in logging output")

-- | A path to a file to load the key-table from
keyTableP :: Parser KeyTableCfg
keyTableP = option (CustomTable <$> str)
  (  long  "key-table"
  <> short 'T'
  <> value EnUS
  <> help  "Custom keyname table file to use instead of KMonad default."
  )

{- NOTE: model configuration options ------------------------------------------}

-- | Flag that, when passed, enabled shell command execution by KMonad
enableCmdP :: Parser Bool
enableCmdP = switch
  (  long  "allow-cmd"
  <> short 'c'
  <> help  "Enable shell-command execution in KMonad"
  )

-- | Flag that, when passed, disables uncaught event rethrowing.
disableFallthroughP :: Parser Bool
disableFallthroughP = flag True False
  (  long  "no-fallthrough"
  <> short 'f'
  <> help  "Disable rethrowing uncaught events"
  )

-- | Passable option to change compose key
composeKeyP :: Parser Keyname
composeKeyP = strOption
  (  long  "compose"
  <> value "ralt"
  <> help  "Key to use for compose-key macro-sequences."
  )

-- | Passable option to change macro-delay
macroDelayP :: Parser Ms
macroDelayP = option (fi <$> (auto :: ReadM Int))
  (  long  "macro-delay"
  <> value 10
  <> help  "Number of milliseconds to pause between keypresses in macros"
  )

-- | Collect all model-configuration options into 1 collection
modelCfgP :: Parser ModelCfg
modelCfgP = ModelCfg <$> enableCmdP
                     <*> disableFallthroughP
                     <*> composeKeyP
                     <*> macroDelayP

{- NOTE: input configuration parsing ------------------------------------------}

-- | Parse a token-name:extra-cfg style string as an input token
--
-- Valid values:
-- - evdev
-- - evdev:/path/to/file
-- - llhook
-- - iokit
-- - iokit:name-pattern
inputTokenP :: Parser InputToken
inputTokenP = option (maybeReader f)
  (  long  "input"
  <> short 'i'
  <> value def
  <> help  h
  )
  where
    f s = case break (== ':') s of
      ("evdev", "")    -> Just $ Evdev (EvdevCfg Nothing)
      ("evdev", ':':s) -> Just $ Evdev (EvdevCfg (Just s))
      ("llhook", "")   -> Just $ LLHook
      ("iokit", "")    -> Just $ IOKit
      -- ("iokit", ':':s) -> Just $ IOKit (Just $ pack s)
      _                -> Nothing
    h = mconcat
        [ "String describing how to capture the keyboard. Pattern: `name:arg` "
        , "where `name` in [evdev, llhook, iokit] and `arg` an extra input for "
        , "`evdev`, where it is the file to open, and `iokit` where it is the "
        , "keyboard name. Examples: evdev, evdev:/dev/input/event0, llhook, "
        , "iokit, iokit:my-kb" ]

-- | Configureable startup delay
startDelayP :: Parser Int
startDelayP = option (fi <$> (auto :: ReadM Int))
  (  long  "start-delay"
  <> short 's'
  <> value 500
  <> help  "Number of milliseconds to pause before grabbing the keyboard"
  )

-- | Full input configuration parser
inputCfgP :: Parser InputCfg
inputCfgP = InputCfg <$> inputTokenP <*> startDelayP

{- NOTE: output configuration parsing ------------------------------------------}

-- | Parse a token-name:extra-cfg style string as an output token
--
-- Valid values:
-- - uinput
-- - uinput:name
-- - sendkeys
-- - ext
outputTokenP :: Parser OutputToken
outputTokenP = option (maybeReader f)
  (  long "output"
  <> short 'o'
  <> value def
  <> help h
  )
  where
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
keyRepeatCfgP :: Parser KeyRepeatCfg
keyRepeatCfgP = option (maybeReader f)
  (  long  "key-repeat"
  <> short 'r'
  <> value def
  <> help h
  )
  where
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

-- | The full output configuration parser
outputCfgP :: Parser OutputCfg
outputCfgP = OutputCfg <$> outputTokenP <*> keyRepeatCfgP

{- NOTE: config-file configuration --------------------------------------------}

-- | Pass a FilePath to a configuration file to load
parseCfgP :: Parser ParseCfg
parseCfgP = ParseCfg <$> option (InRoot <$> str)
  (  long    "config"
  <> short   'f'
  <> metavar "FILE"
  <> value   def
  <> help    "The kmonad configuration file to load."
  )

{- NOTE: ungrouped parsers ----------------------------------------------------}

dumpEnUSP :: Parser Bool
dumpEnUSP = switch
  (  long "dump-table"
  <> help "If provided, print the standard keytable and exit")
