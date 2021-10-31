{-# LANGUAGE QuasiQuotes #-}
module KMonad.App.Invocation.IO
  ( getInvocation )
where

import Data.Version
import KMonad.Prelude
import KMonad.App.Invocation.Parser
import KMonad.App.Invocation.TH
import KMonad.App.Types
import KMonad.App.Configurable
import Options.Applicative
import Text.RawString.QQ

import Paths_kmonad (version)



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
getInvocation :: OnlyIO ReCfg
getInvocation = customExecParser (prefs showHelpOnEmpty) $
  info (invocationP <**> versioner <**> helper)
    (  fullDesc
    <> progDesc (unpack greeting)
    <> header   "kmonad - an onion of buttons."
    )

