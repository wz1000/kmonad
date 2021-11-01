module KMonad.App.Invocation.Parser
  ( invocationP )
where

import KMonad.Prelude

import KMonad.App.Configurable
import KMonad.App.Types
import KMonad.Util.Name

import Options.Applicative

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

{- SECTION: Types and utilities -----------------------------------------------}

type P = Parser CfgChange

-- | Extra mods to apply to specific flags
fMods :: Named (Mod FlagFields CfgChange)
fMods = M.fromList
  [ ( "verbose"      , short 'v' )
  , ( "commands-off" , short 's' )
  ]

-- | Extra mods to apply to specific options
oMods :: Named (Mod OptionFields CfgChange)
oMods = M.fromList
  [ ( "config-file" , short 'f' <> metavar "FILE" )
  , ( "key-table"   , short 'k' <> metavar "FILE")
  , ( "log-level"   , short 'l' )
  , ( "start-delay" , short 'd' )
  , ( "compose-key" , metavar "KEYNAME" )
  ]

-- | Automatically create a command-line flag from an 'AnyFlag' value
fromFlag :: AnyFlag -> P
fromFlag f = flag mempty (runAnyFlag f)
  (  fMods ^. ix (f^.name) -- NOTE: we rely on Mod's monoid when value not found
  <> long (unpack $ f^.name)
  <> help (unpack $ f^.description))

-- | Automatically create a command-line option from an 'Option' value
fromOption :: AnyOption -> P
fromOption o = option (eitherReader $ left show . runAnyOption o . pack)
  (  oMods ^. ix (o^.name) -- NOTE: we rely on Mod's monoid when value not found
  <> value mempty
  <> long (unpack $ o^.name)
  <> help (unpack $ o^.description))

-- | Turn a bunch of P's into a single P by applying each edit in order.
--
-- Note that rightmost is foremost, i.e. entries more to the front of the list
-- can overwrite changes made by entries behind them, but not vice-versa. This
-- is defined by the Monoid of Endo.
pConcat :: [P] -> P
pConcat ps = mconcat <$> sequenceA ps

{- SECTION: Parsers -----------------------------------------------------------}

{- SUBSECTION: Top-level and task parsers -------------------------------------}

-- | Parse the top-level change to the default RootCfg passed on the cmd-line
invocationP :: P
invocationP = pConcat [ rootCfgP, taskP ]

-- | Parse the task as a subcommand
taskP :: P
taskP = hsubparser $ mconcat
  [ mkOne "run" "Run a keyboard remapping."
      (Run def) [modelCfgP, inputCfgP, outputCfgP]
  , mkOne "discover" "Interactively explore button keycodes and names."
      (Discover def) [inputCfgP, discoverCfgP]
  , mkOne "parse-test" "Check a config-file for errors."
      ParseTest []
  ]
  where
    mkOne :: Name -> Description -> Task -> [P] -> (Mod CommandFields CfgChange)
    mkOne n d t ps =
      let p_  = pConcat $ ps <> [rootCfgP] <> [pure tsk]
          -- ^ Concat all parsers into 1, *order is important*
          tsk = mkChange ("task:" <> n) (\c -> c & task .~ t)
          -- ^ Create a task for the provided settings
      in (command (unpack n) (info p_ (progDesc (unpack d))))

-- {- SUBSECTION: Parsers by category --------------------------------------------}

-- | Helper function to compile flags and options into parser
mkCfg :: Named AnyFlag -> Named AnyOption -> P
mkCfg fs os = pConcat $ (map fromFlag   . M.elems $ fs)
                     <> (map fromOption . M.elems $ os)

-- | Different parsers per category
--
-- Note, I could move modelCfgP and outputCfgP to a new runCfgP because they are
-- currently only used by the 'Run' task, but I anticipate a few more tasks that
-- might/will need access to these settings. So the seperation has been made for
-- clarify of context and in preparation for these developments.
rootCfgP, modelCfgP, inputCfgP, outputCfgP, discoverCfgP :: P
rootCfgP    = mkCfg basicFlags    basicOptions
modelCfgP    = mkCfg modelFlags    modelOptions
inputCfgP    = mkCfg inputFlags    inputOptions
outputCfgP   = mkCfg outputFlags   outputOptions
discoverCfgP = mkCfg discoverFlags discoverOptions
