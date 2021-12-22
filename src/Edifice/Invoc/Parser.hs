-- |

module Edifice.Invoc.Parser where

import Preface

import Art
import Util.Configurable
import Edifice.Invoc.Types

import Options.Applicative

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

{- SECTION: Types -------------------------------------------------------------}

-- | Parser that parser changes to some structure
type P s = Parser (Changes s)

-- | Top level parser that parses some update to Invoc
type Top = Parser (Invoc -> Invoc)

{- SECTION: Data --------------------------------------------------------------}

-- | Extra mods to apply to specific flags
fMods :: Named (Mod FlagFields a)
fMods = M.fromList
  [ ( "verbose"      , short 'v' )
  , ( "commands-off" , short 's' )
  ]

-- | Extra mods to apply to specific options
oMods :: Named (Mod OptionFields a)
oMods = M.fromList
  [ ( "config-file" , short 'f' <> metavar "FILE" )
  , ( "key-table"   , short 'k' <> metavar "FILE")
  , ( "log-level"   , short 'l' )
  , ( "start-delay" , short 'd' )
  , ( "compose-key" , metavar "KEYNAME" )
  ]

{- SECTION: Ops ---------------------------------------------------------------}

-- | Automatically create a command-line flag from an 'AnyFlag' value
fromFlag :: AnyFlag s -> P s
fromFlag f = flag mempty (runAnyFlag f)
  (  fMods ^. ix (f^.name) -- NOTE: we rely on Mod's monoid when value not found
  <> long (unpack $ f^.name)
  <> help (unpack $ f^.description))

-- | Automatically create a command-line option from an 'Option' value
fromOption :: AnyOption s -> P s
fromOption o = option (eitherReader $ left show . runAnyOption o . pack)
  (  oMods ^. ix (o^.name) -- NOTE: we rely on Mod's monoid when value not found
  <> value mempty
  <> long (unpack $ o^.name)
  <> help (unpack $ o^.description))

-- | Collapse a collection of parsers of changes into 1 big parser of many
-- changes.
--
-- Note that rightmost is foremost, i.e. entries more to the front of the list
-- can overwrite changes made by entries behind them, but not vice-versa. This
-- is defined by the Monoid of Endo.
pConcat :: (Monoid a, Traversable t, Applicative f) => t (f a) -> f a
pConcat ps = mconcat . toList <$> sequenceA ps

-- | Create a top-level parser that collects changes to a particular structure
mkTop :: ()
  => Traversal' Invoc (Changes s) -- ^ How the substructure is embedded in the Invoc
  -> Named (AnyFlag s)            -- ^ Flags on the substructure
  -> Named (AnyOption s)          -- ^ Options on the substructure
  -> Top                          -- ^ Parser that returns an Invoc update
mkTop l fs os =
  let p = pConcat $ (map fromFlag   . M.elems $ fs)
                 <> (map fromOption . M.elems $ os)
  in (\c i -> i & l .~ c) <$> p

logCfgP :: Parser (Change Invoc)
logCfgP = mkTop logChanges logFlags logOpts

-- -- | Parse a task and a list of other top-level parsers to run
-- taskP :: Parser Task
-- taskP = hsubparser $ mconcat
--     [ mkOne Run "run"
--       "Run a keyboard remapping."
--     , mkOne Discover "discover"
--       "Interactively explore button keycodes and names."
--     , mkOne ParseTest "parsetest"
--       "Check a configuration file for validity"
--     ]
--     where
--       mkOne :: Name -> Task -> Description -> (Mod CommandFields (Task, [Top]))
--       mkOne n d t ps = command (unpack n) (info (pure t) (progDesc (unpack d)))

-- invocP :: Top
invocP = hsubparser . mconcat $
  [ z Run "run" []
      "Run a keyboard remapping."
  , z Discover "discover" []
      "Interactively explore button keycodes and names."
  , z ParseTest "parsetest" []
      "Check a configuration file for validity"
  ]
  where
    z :: Task -> Name -> [Top] -> Description -> (Mod CommandFields (Changes Invoc))
    z t n ps d =
      let p_  = pConcat $ ps <> [ logCfgP ] <> [ pure tsk ]
          -- ^ Concat all parsers into 1, *order is important*
          tsk = mkChange ("task:" <> n) (\c -> c & task .~ t)
          -- ^ Create a task for the provided settings
      in (command (unpack n) (info p_ (progDesc (unpack d))))
      command (unpack n) (info )

  -- f <$> taskP
  -- where f t ps = pConcat ps $ (def & task .~ t)

--   -- [ mkOne "run" "Run a keyboard remapping."
--   --     (Run def) [modelCfgP, inputCfgP, outputCfgP, localeCfgP]
--   -- , mkOne "discover" "Interactively explore button keycodes and names."
--   --     (Discover def) [inputCfgP, discoverCfgP, localeCfgP]
--   -- , mkOne "parse-test" "Check a config-file for errors."
--   --     ParseTest []
--   -- ]


--   where
--     mkOne :: Name -> Description -> Task -> [P] -> (Mod CommandFields CfgChange)
--     mkOne n d t ps =
--       let p_  = pConcat $ ps <> [generalCfgP] <> [pure tsk]
--           -- ^ Concat all parsers into 1, *order is important*
--           tsk = mkChange ("task:" <> n) (\c -> c & task .~ t)
--           -- ^ Create a task for the provided settings
--       in (command (unpack n) (info p_ (progDesc (unpack d))))


-- invocationP :: Parser Invoc
-- invocationP = hsub

--   where f =

--   let stdP = [ logCfgP ] in
--   hsubparser $ mconcat
--     [ mkOne "run" "Run a keyboard remapping."
--       Run

--     ]

--     where
--       mkOne :: Name -> Description -> Task ->

-- --   where
-- --     mkOne :: Name -> Description -> Task -> [P] -> (Mod CommandFields CfgChange)
-- --     mkOne n d t ps =
-- --       let p_  = pConcat $ ps <> [generalCfgP] <> [pure tsk]
-- --           -- ^ Concat all parsers into 1, *order is important*
-- --           tsk = mkChange ("task:" <> n) (\c -> c & task .~ t)
-- --           -- ^ Create a task for the provided settings
-- --       in (command (unpack n) (info p_ (progDesc (unpack d))))





-- -- | Parse the task as a subcommand
-- -- taskP :: P
-- -- taskP = hsubparser $ mconcat
-- --   [ mkOne "run" "Run a keyboard remapping."
-- --       (Run def) [modelCfgP, inputCfgP, outputCfgP, localeCfgP]
-- --   , mkOne "discover" "Interactively explore button keycodes and names."
-- --       (Discover def) [inputCfgP, discoverCfgP, localeCfgP]
-- --   , mkOne "parse-test" "Check a config-file for errors."
-- --       ParseTest []
-- --   ]
