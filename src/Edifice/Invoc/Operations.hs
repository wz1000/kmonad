{-# LANGUAGE QuasiQuotes #-}
-- |

module Edifice.Invoc.Operations where

import Preface
import Util.Configurable
import Art

import Edifice.Invoc.Parser
import Edifice.Invoc.TH
import Edifice.Invoc.Types

import Options.Applicative
import Text.RawString.QQ

{- SECTION: Operations --------------------------------------------------------}

-- | Extract a 'LogCfg' from an 'Invoc'
getLogCfg :: Invoc -> LogCfg
getLogCfg = onDef . view logChanges

{- SECTION: IO ----------------------------------------------------------------}

greeting :: Text
greeting = [r|

Remap keys to other keys, see more here:
https://github.com/kmonad/kmonad.git

|]

-- | Equip a parser with version information about the program
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version <> ", commit " <> $(gitHash))
  (  long "version"
  <> short 'V'
  <> help "Show version"
  )

-- | Parse exactly how this program was invoked.
getInvoc :: OnlyIO Invoc
getInvoc = customExecParser (prefs showHelpOnEmpty) $
  info (invocP <**> versioner <**> helper)
    (  fullDesc
    <> progDesc (unpack greeting)
    <> header   "kmonad - an onion of buttons."
    )
