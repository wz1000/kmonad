module KMonad.App.Parser.Operations

where

import KMonad.Prelude hiding (try)

import KMonad.App.Types
import KMonad.App.Operations
import KMonad.App.Parser.Types

import System.Keyboard

import Text.Megaparsec

-- | Run a parser on some text and return the result
--
-- Note that this will throw any parse-error it encounters
runParse :: CanBasic m env => ParseCfg -> P a -> Text -> m a
runParse c p t = do
  tbl     <- view keyTable
  cmpcode <- view (codeForName $ c^.composeKey) >>= \case
    Nothing -> throwIO $ UnknownComposeKey (c^.composeKey)
    Just c  -> pure c
  sftcode <- view (codeForName $ "stdsft") >>= \case
    Nothing -> throwIO $ UnknownShiftKey "stdsft"
    Just c  -> pure c


  let env = ParseEnv
        { _pKeyTable   = tbl
        , _composeCode = cmpcode
        , _shiftCode   = sftcode
        }

  case runReader (runParserT p "" t) env of
    Left e  -> throwIO $ PErrors e
    Right a -> pure a

-- | Shorthand for debugging, maybe delete later
--
-- Very handy in the REPL, e.g.
-- >> prs bool "true"
-- True
prs :: P a -> Text -> OnlyIO a
prs p = runBasic def . runParse def p


-- withParse :: CanBasic m env => ParseCfg -> ()

-- -- | Like 'fromLexicon' but case-insensitive
-- fromLexicon' :: Lexicon a -> Parser a
-- fromLexicon' = choice . map go . sortBy f . M.toList where
--   f  (k, _) (l, _) = compare l k     -- ^ Reverse sort by name
--   go (k, v) = v <$ (try $ string' k) -- ^ Match the name and insert the value

-- -- | Turn a Lexicon of names items into a parser
-- fromLexicon :: Lexicon a -> Parser a
-- fromLexicon = choice . map go . sortBy f . M.toList where
--   f  (k, _) (l, _) = compare l k    -- ^ Reverse sort by name
--   go (k, v) = v <$ (try $ string k) -- ^ Match the name and insert the value



-- -- thing :: [(Text, Text)]
-- -- thing = zip $
-- -- thing = over (both.traversed) (+1) ([1, 2], [8, 7])
-- thing :: [(Text, Text)]
-- thing = uncurry zip $ over (both.traversed) T.singleton ("abba", "1234")
