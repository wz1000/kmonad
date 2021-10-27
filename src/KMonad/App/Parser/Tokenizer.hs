{-|
Module      : KMonad.Args.Parser
Description : How to turn a text-file into config-tokens
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

We perform configuration parsing in 2 steps:
- 1. We turn the text-file into a token representation
- 2. We check the tokens and turn them into an AppCfg

This module covers step 1.

-}
module KMonad.App.Parser.Tokenizer
  ( -- * Parsing 'KExpr's
  --   parseTokens
  -- , loadTokens

  -- -- * Building Parsers
  -- , symbol
  -- , numP

  -- -- * Parsers for Tokens and Buttons
  -- , otokens
  -- , itokens
  -- , keywordButtons
  -- , noKeywordButtons
  )
where

import KMonad.Prelude hiding (try, bool)

import KMonad.App.Types
import KMonad.App.Parser.Keycode
import KMonad.App.Parser.Operations
import KMonad.App.Parser.Types
import KMonad.App.KeyIO
import KMonad.Util.Keyboard

import Data.Char
import RIO.List (sortBy, find)


import qualified RIO.HashMap as M
import qualified RIO.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char

import System.Keyboard

{- SECTION: Top-level ---------------------------------------------------------}


-- | Try to parse an entire configuration
-- parseConfig :: ParseCfg -> Text -> Either PErrors [KExpr]
-- parseConfig = flip parse configP

-- | Top level parser
configP :: P [KExpr]
configP = undefined

{- SECTION: Elementary parsers ------------------------------------------------}

-- | Parse any amount of whitespace
sc :: P ()
sc = L.space
  space1
  (L.skipLineComment  ";;")
  (L.skipBlockComment "#|" "|#")

-- | Turn a parser into one that consumes all whitespace behind it.
lexeme :: P a -> P a
lexeme = L.lexeme sc

-- | Match 1 literal symbol (and consume trailing whitespace)
symbol :: Text -> P ()
symbol = void . L.symbol sc

-- | List of all characters that /end/ a word or sequence
terminators :: String
terminators = ")\""

terminatorP :: P Char
terminatorP = satisfy (`elem` terminators)

-- | Consume all chars until a space is encounterd
word :: P Text
word = T.pack <$> some (satisfy wordChar)
  where wordChar c = not (isSpace c || c `elem` terminators)

-- | Run the parser IFF it is followed by a space, eof, or reserved char
terminated :: P a -> P a
terminated p = try $ p <* lookAhead (void spaceChar <|> eof <|> void terminatorP)

-- | Run the parser IFF it is not followed by a space or eof.
prefix :: P a -> P a
prefix p = try $ p <* notFollowedBy (void spaceChar <|> eof)

-- | Create a parser that matches symbols to values and only consumes on match.
fromNamed :: [(Text, a)] -> P a
fromNamed = choice . map mkOne . srt
  where
    -- | Sort descending by length of key and then alphabetically
    srt :: [(Text, b)] -> [(Text, b)]
    srt = sortBy . flip on fst $ \a b ->
      case compare (T.length b) (T.length a) of
        EQ -> compare a b
        x  -> x

    -- | Make a parser that matches a terminated symbol or fails
    mkOne (s, x) = terminated (string s) *> pure x

-- | Run a parser between 2 sets of parentheses
paren :: P a -> P a
paren = between (symbol "(") (symbol ")")

-- | Run a parser between 2 sets of parentheses starting with a symbol
statement :: Text -> P a -> P a
statement s = paren . (symbol s *>)

-- | Run a parser that parser a bool value
bool :: P Bool
bool = symbol "true" *> pure True
   <|> symbol "false" *> pure False

-- | Parse a LISP-like keyword of the form @:keyword value@
keywordP :: Text -> P p -> P p
keywordP kw p = lexeme (string (":" <> kw)) *> lexeme p
  <?> "Keyword " <> ":" <> T.unpack kw

--------------------------------------------------------------------------------
-- $elem
--
-- Parsers for elements that are not stand-alone KExpr's

-- | Parse an integer
numP :: P Int
numP = L.decimal

-- | Parse text with escaped characters between "s
textP :: P Text
textP = do
  _ <- char '\"' <|> char '\''
  s <- manyTill L.charLiteral (char '\"' <|> char '\'')
  pure . T.pack $ s

-- | Parse a variable reference
derefP :: P Text
derefP = prefix (char '@') *> word

-- --------------------------------------------------------------------------------
-- -- $cmb
-- --
-- -- Parsers built up from the basic KExpr's

-- -- | Consume an entire file of expressions and comments
-- configP :: P [KExpr]
-- configP = sc *> exprsP <* eof

-- -- | Parse 0 or more KExpr's
-- exprsP :: P [KExpr]
-- exprsP = lexeme . many $ lexeme exprP

-- -- | Parse 1 KExpr
-- exprP :: P KExpr
-- exprP = paren . choice $
--   [ try (symbol "defcfg")   *> (KDefCfg   <$> defcfgP)
--   , try (symbol "defsrc")   *> (KDefSrc   <$> defsrcP)
--   , try (symbol "deflayer") *> (KDefLayer <$> deflayerP)
--   , try (symbol "defalias") *> (KDefAlias <$> defaliasP)
--   ]

-- -- -- | Parse a (123, 456) tuple as a KeyRepeatCfg
-- -- repCfgP :: P KeyRepeatCfg
-- -- repCfgP = lexeme $ paren $ do
-- --   a <- numP
-- --   _ <- char ','
-- --   b <- numP
-- --   pure $ KeyRepeatCfg (fi a) (fi b)

-- --------------------------------------------------------------------------------
-- $but
--
-- All the various ways to refer to buttons

-- | Turn 2 strings into a list of singleton-Text tuples by zipping the lists.
--
-- z "abc" "123" -> [("a", "1"), ("b", 2) ...]
z :: String -> String -> [(Text, Text)]
z a b = uncurry zip $ over (both.traversed) T.singleton (a, b)

-- | Make a button that emits a particular keycode
emitOf :: Keyname -> P DefButton
emitOf n = do
  view (codeForName n) >>= \case
    Nothing -> customFailure $ NoKeycodeFor n
    Just c  -> pure $ KEmit c

-- | A parser that parses any @shifted@ name from the keycode table
shiftedP :: P DefButton
shiftedP = undefined



-- | Make a button that emits a particular shifted keycode
-- shiftedOf :: CoreName -> DefButton
-- shiftedOf = KAround (emitOf "lsft") . emitOf

-- | Different ways to refer to shifted versions of keycodes
-- shiftedNames :: [(Text, DefButton)]
-- shiftedNames = map (second (shiftedOf . CoreName)) $ cps <> num <> oth where
--   cps = z ['A'..'Z'] ['a'..'z']
--   num = z "!@#$%^&*" "12345678"
--   oth = z "<>:~\"|{}+?" -- NOTE: \" is an escaped " and lines up with '
--           ",.;`'\\[]=/" -- NOTE: \\ is an escaped \ and lines up with |

-- -- | Create a button that emits some keycode while holding shift
-- shiftedB :: CoreName -> DefButton
-- shiftedB = KAround (KEmit $ kc "lsft") . KEmit . kc

-- -- | Names for various buttons
-- buttonNames :: [(Text, DefButton)]
-- buttonNames = shiftedNames <> escp <> util
--   where
--     -- Escaped versions for reserved characters
--     escp = [ ("\\(", shiftedB "9"), ("\\)", shiftedB "0")
--            , ("\\_", shiftedB "-"), ("\\\\", KEmit $ kc "\\")]
--     -- Extra names for useful buttons
--     util = [ ("_", KTrans), ("XX", KBlock)
--            , ("lprn", shiftedB "9"), ("rprn", shiftedB "0")]

-- -- | Parse "X-b" style modded-sequences
-- moddedP :: P DefButton
-- moddedP = KAround <$> prfx <*> buttonP
--   where mods = [ ("S-",  kc "lsft"), ("C-",  kc "lctl")
--                , ("A-",  kc "lalt"), ("M-",  kc "lmet")
--                , ("RS-", kc "rsft"), ("RC-", kc "rctl")
--                , ("RA-", kc "ralt"), ("RM-", kc "rmet")]
--         prfx = choice $ map (\(t, p) -> prefix (string t) *> pure (KEmit p)) mods

-- -- | Parse Pxxx as pauses (useful in macros)
-- pauseP :: P DefButton
-- pauseP = KPause . fromIntegral <$> (char 'P' *> numP)

-- -- | #()-syntax tap-macro
-- rmTapMacroP :: P DefButton
-- rmTapMacroP =
--   char '#' *> paren (KTapMacro <$> some buttonP
--                                <*> optional (keywordP "delay" numP))

-- -- | Compose-key sequence
-- composeSeqP :: P [DefButton]
-- composeSeqP = do
--   -- Lookup 1 character in the compose-seq list
--   c <- anySingle <?> "special character"
--   s <- case find (\(_, c', _) -> (c' == c)) ssComposed of
--          Nothing -> fail "Unrecognized compose-char"
--          Just b  -> pure $ b^._1

--   -- If matching, parse a button-sequence from the stored text
--   --
--   -- NOTE: Some compose-sequences contain @_@ characters, which would be parsed
--   -- as 'Transparent' if we only used 'buttonP', that is why we are prefixing
--   -- that parser with one that check specifically and only for @_@ and matches
--   -- it to @shifted min@

--   let underscore = shiftedB "-" <$ lexeme (char '_')

--   case runParser (some $ underscore <|> buttonP) "" s of
--     Left  _ -> fail "Could not parse compose sequence"
--     Right b -> pure b

-- -- | Parse a dead-key sequence as a `+` followed by some symbol
-- deadkeySeqP :: P [DefButton]
-- deadkeySeqP = do
--   _ <- prefix (char '+')
--   c <- satisfy (`elem` ("~'^`\"," :: String))
--   case runParser buttonP "" (T.singleton c) of
--     Left  _ -> fail "Could not parse deadkey sequence"
--     Right b -> pure [b]

-- -- | Parse any button
-- buttonP :: P DefButton
-- buttonP = (lexeme . choice . map try $
--   map (uncurry statement) keywordButtons ++ noKeywordButtons
--   ) <?> "button"

-- -- | Parsers for buttons that have a keyword at the start; the format is
-- -- @(keyword, how to parse the token)@
-- keywordButtons :: [(Text, P DefButton)]
-- keywordButtons =
--   [ ("around"         , KAround      <$> buttonP     <*> buttonP)
--   , ("multi-tap"      , KMultiTap    <$> timed       <*> buttonP)
--   , ("tap-hold"       , KTapHold     <$> lexeme numP <*> buttonP <*> buttonP)
--   , ("tap-hold-next"  , KTapHoldNext <$> lexeme numP <*> buttonP <*> buttonP)
--   , ("tap-next-release"
--     , KTapNextRelease <$> buttonP <*> buttonP)
--   , ("tap-hold-next-release"
--     , KTapHoldNextRelease <$> lexeme numP <*> buttonP <*> buttonP)
--   , ("tap-next"       , KTapNext     <$> buttonP     <*> buttonP)
--   , ("layer-toggle"   , KLayerToggle <$> lexeme word)
--   , ("layer-switch"   , KLayerSwitch <$> lexeme word)
--   , ("layer-add"      , KLayerAdd    <$> lexeme word)
--   , ("layer-rem"      , KLayerRem    <$> lexeme word)
--   , ("layer-delay"    , KLayerDelay  <$> lexeme numP <*> lexeme word)
--   , ("layer-next"     , KLayerNext   <$> lexeme word)
--   , ("around-next"    , KAroundNext  <$> buttonP)
--   , ("around-next-timeout", KAroundNextTimeout <$> lexeme numP <*> buttonP <*> buttonP)
--   , ("tap-macro"
--     , KTapMacro <$> lexeme (some buttonP) <*> optional (keywordP "delay" numP))
--   , ("tap-macro-release"
--     , KTapMacroRelease <$> lexeme (some buttonP) <*> optional (keywordP "delay" numP))
--   , ("cmd-button"     , KCommand     <$> lexeme textP <*> optional (lexeme textP))
--   , ("pause"          , KPause . fromIntegral <$> numP)
--   , ("sticky-key"     , KStickyKey   <$> lexeme numP <*> buttonP)
--   ]
--  where
--   timed :: P [(Int, DefButton)]
--   timed = many ((,) <$> lexeme numP <*> lexeme buttonP)

-- -- | Parsers for buttons that do __not__ have a keyword at the start
-- noKeywordButtons :: [P DefButton]
-- noKeywordButtons =
--   [ KComposeSeq <$> deadkeySeqP
--   , KRef  <$> derefP
--   , lexeme $ fromNamed buttonNames
--   , try moddedP
--   , lexeme $ try rmTapMacroP
--   , lexeme $ try pauseP
--   , KEmit <$> keycodeP
--   , KComposeSeq <$> composeSeqP
--   ]

-- --------------------------------------------------------------------------------
-- -- $defcfg

-- -- | Parse an input token
-- itokenP :: P InputToken
-- itokenP = choice $ map (try . uncurry statement) itokens

-- -- | Input tokens to parse; the format is @(keyword, how to parse the token)@
-- itokens :: [(Text, P InputToken)]
-- itokens =
--   [ ("device-file"   , Evdev . fmap unpack <$> optional textP)
--   , ("low-level-hook", pure LLHook)
--   , ("iokit-name"    , IOKit <$> optional textP)]

-- -- | Parse an output token
-- otokenP :: P OutputToken
-- otokenP = choice $ map (try . uncurry statement) otokens

-- -- | Output tokens to parse; the format is @(keyword, how to parse the token)@
-- otokens :: [(Text, P OutputToken)]
-- otokens =
--   [ ("uinput-sink"    , Uinput <$> lexeme (optional textP) <*> lexeme (optional textP))
--   , ("send-event-sink", pure SendKeys)
--   , ("dext"           , pure Ext)
--   , ("kext"           , pure Ext)]

-- -- | Parse the DefCfg token
-- defcfgP :: P DefSettings
-- defcfgP = some (lexeme settingP)

-- -- | All the settable settings in a `defcfg` block
-- settings :: [(Text, P DefSetting)]
-- settings =
--     [ ("input"         , SIToken      <$> itokenP)
--     , ("output"        , SOToken      <$> otokenP)
--     , ("cmp-seq-delay" , SCmpSeqDelay <$> numP)
--     , ("cmp-seq"       , SCmpSeq      <$> buttonP)
--     , ("init"          , SInitStr     <$> textP)
--     , ("fallthrough"   , SFallThrough <$> bool)
--     , ("allow-cmd"     , SAllowCmd    <$> bool)
--     ]

-- -- | All possible configuration options that can be passed in the defcfg block
-- settingP :: P DefSetting
-- settingP = lexeme . choice . map (\(s, p) -> (try $ symbol s) *> p) $ settings


-- --------------------------------------------------------------------------------
-- -- $defalias

-- -- | Parse a collection of names and buttons
-- defaliasP :: P DefAlias
-- defaliasP = many $ (,) <$> lexeme word <*> buttonP

-- --------------------------------------------------------------------------------
-- -- $defsrc

-- defsrcP :: P DefSrc
-- defsrcP = many $ lexeme keycodeP

-- --------------------------------------------------------------------------------
-- -- $deflayer
-- deflayerP :: P DefLayer
-- deflayerP = DefLayer <$> lexeme word <*> many (lexeme buttonP)
