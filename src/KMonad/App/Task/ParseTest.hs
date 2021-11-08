-- |

module KMonad.App.Task.ParseTest where

import Preface

import KMonad.App.CfgFile
import KMonad.App.IO
import KMonad.App.Logging

import KMonad.App.Configurable

data ParseTestException = NoCfgFile
  deriving (Eq, Show)

instance Exception ParseTestException where
  displayException NoCfgFile =
    "Could not find config to parse, none provided and none in config-dir"

-- | Run a parse on the 'CfgFile', then exit
runParseTest :: CanRoot m env => m ()
runParseTest = getCfgFile >>= \case
  Nothing -> throwIO NoCfgFile
  Just f  -> do
    atWarn $ sep >> (log $ "Testing parse of: " <> pack f)
    txt <- readFileUtf8 f
    tks <- either throwIO pure $ tokenize txt
    atInfo $ sep >> (log $ "Tokenized representation: ") >> pp tks
    chg <- either throwIO pure $ grok tks
    atInfo $ sep >> (log $ "Grokked changes: ") >> pp (chg^..folded.description)
    atWarn . log $ "Parse succesful"
