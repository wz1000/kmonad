-- |

module KMonad.App.Parser.Grokker where

import KMonad.Prelude

import KMonad.App.Configurable
import KMonad.App.Parser.Tokenizer
import KMonad.App.Types
import KMonad.Util.Name

import System.Keyboard -- FIXME: delete when done
import KMonad.App.Parser.Test -- FIXME: Remove when done


{- NOTE: ordering --------------------------------------------------------------
- To properly grok buttons and defsrc we need access to a KeyTable
- To get access to a KeyTable we need a Basic context
- To get a Basic context we need to apply some of the settings (like KeyTable)
- To get all configurables we need to grok at least part of the `defcfg` blocks

ERGO: We need grokking to be a 2 phase process:
1. Base context, extract all defcfg blocks, grok into Change BasicCfg
2. Basic context, parse the other blocks using the now present keytable
-------------------------------------------------------------------------------}

{- SECTION: errors ------------------------------------------------------------}

data GrokException
  = NoGrokConfigurable ConfigurableException
  deriving (Eq, Show)

{- SECTION: extract configureables --------------------------------------------}

-- | Turn all KSettings in a collection of tokens into a config-update.
grokSettings :: KTokens -> Either ConfigurableException ReCfg
grokSettings ks = do
  fs <- foldMapM grokFlag   $ ks^..folded._KCfg.folded.uKSetting._Left
  os <- foldMapM grokOption $ ks^..folded._KCfg.folded.uKSetting._Right
  pure $ fs <> os

-- | Turn a flag name into an edit of the BasicCfg
grokFlag :: FlagName -> Either ConfigurableException ReCfg
grokFlag f = case allFlags ^. at f of
  Nothing -> Left  $ UnknownConfigurable f
  Just f  -> Right $ runAnyFlag f

-- | Turn an option (name, value) pair into an edit of the BasicCfg
grokOption :: (OptionName, Text) -> Either ConfigurableException ReCfg
grokOption (o, t) = case allOptions ^. at o of
  Nothing -> Left $ UnknownConfigurable o
  Just o_ -> runAnyOption o_ t


gfull :: OnlyIO ()
gfull = do
  tk <- prs ktokens myCfg
  case grokSettings tk of
    Left e -> throwIO e
    Right a -> pp $ a^.changes


