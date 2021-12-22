-- |

module Edifice.Ctx.ParseTest

where

import Util
import Art
import Edifice.Ctx.Types
-- import Edifice.CfgFile
-- import Edifice.Types



data ParseTestException = NoCfgFile
  deriving (Eq, Show)

instance Exception ParseTestException where
  displayException NoCfgFile =
    "Could not find config to parse, none provided and none in config-dir"

runParseTest :: D ()
runParseTest = atError $ log "parse text!"

-- | Run a parse on the 'CfgFile', then exit
-- runParseTest :: BaseCtx m env => m ()
-- runParseTest = getCfgFile >>= \case
--   Nothing -> throwIO NoCfgFile
--   Just f  -> do
--     atWarn $ sep >> (log $ "Testing parse of: " <> pack f)
--     txt <- readFileUtf8 f
--     tks <- either throwIO pure $ tokenize txt
--     atInfo $ sep >> (log $ "Tokenized representation: ") >> pp tks
--     chg <- either throwIO pure $ grok tks
--     atInfo $ sep >> (log $ "Grokked changes: ") >> pp (chg^..folded.description)
--     atWarn . log $ "Parse succesful"
