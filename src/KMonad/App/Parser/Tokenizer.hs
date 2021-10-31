{-# LANGUAGE QuasiQuotes #-}
-- |
module KMonad.App.Parser.Tokenizer where


import KMonad.Prelude hiding (try)

import KMonad.App.Configurable
import KMonad.Util.Name
import KMonad.Util.Time
import System.Keyboard

import KMonad.App.Parser.Test -- FIXME: Remove when done

import Data.Char

import Text.RawString.QQ
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import RIO.List (sortBy)
import qualified RIO.Text as T
import RIO.Partial (read)

{- OVERVIEW: what do we do here? -----------------------------------------------

KMonad accepts a configuration file that is used to specify some configurable
settings (also available via invocation, in fact: invocation-passed settings
override config-passed settings) and the keymap that KMonad will apply to the
input key signal.

We allow for a number of different syntax constructs, like
- lisp-like list syntax
- keyword arguments
- string, int, hex literals
- alias definition and reference
- comments

To build up a valid config is an involved task, so in this module, we only take
raw text and turn it into an intermediate representation that is still not
guaranteed to be a valid configration, but is much easier to work with (the next
step is in Grokker).

Here we:
- remove all comments
- parse literals, lists, and dereferences
- separate out the top-level blocks
- only match on valid flag and opt names (but we don't parse the opt args)

-}



{- SECTION: types -------------------------------------------------------------}

{- SUBSECTION: Megaparsec specializations -------------------------------------}

-- | Parser's operate on Text and carry no state
-- type Parser = Parsec Void Text
type P a = Parsec Void Text a

-- | The type of errors returned by the Megaparsec parsers
newtype PErrors = PErrors (ParseErrorBundle Text Void)
  deriving Eq

instance Show PErrors where
  show (PErrors e) = "Parse error at " <> errorBundlePretty e

instance Exception PErrors

{- SUBSECTION: KExprs ---------------------------------------------------------}

newtype KSetting = KSetting { _uKSetting :: Either FlagName (OptionName, Text) }
  deriving (Eq, Show)
makeLenses ''KSetting

instance HasName KSetting where
  name = lens (view $ uKSetting . beside id _2)
              (\s a -> s & uKSetting . beside id _2 .~ a)

-- | The final type delivered by the tokenizer
type KTokens = [KBlock]

-- | Top level config file expressions
data KBlock
  = KCfg   [KSetting]       -- ^ A defcfg block with a series of text entries
  | KAlias [(Text, KExpr)] -- ^ A defalias block with a list of (name, expr) pairs
  | KSrc   [Keyname]       -- ^ A defsrc block with a list of keynames
  | KLayer Text [KExpr]    -- ^ A deflayer block with a name and a list of expr
  deriving (Eq, Show)

-- | KExpr expressions describing actions that KMonad should take
--
-- Note that `legal text` as described below means:
-- any series of non-reserved characters demarcated by whitespace, where
-- reserved-characters = " ' and )
data KExpr
  = KList       Text [KExpr] -- ^ A lisp-like list with a command and some args
  | KKeyLit     Keycode      -- ^ A keycode-literal read from hex
  | KPause      Ms           -- ^ A pause instruction
  | KDeref      Text         -- ^ An alias derefernce
  | KKeyword    Text         -- ^ A colon-prefixed keyword
  | KWord Text               -- ^ Any other bit of legal text
  deriving (Eq, Show)

makePrisms ''KBlock
makePrisms ''KExpr

{- SECTION: Utils -------------------------------------------------------------}

-- | Predicate describing the set of prefix characters
isPrefixChar :: Char -> Bool
isPrefixChar = (`elem` ("@#:" :: String))

-- | Predicate describing the set of reserved characters
isReservedChar :: Char -> Bool
isReservedChar = (`elem` ("\"')" :: String))

-- | Predicate describing the set of word-legal characters
isWordChar :: Char -> Bool
isWordChar c = not (isSpace c || isReservedChar c)

{- SECTION: basic parser combinators ------------------------------------------}

-- | Consume whitespace and comments
sc :: P ()
sc = L.space
  space1
  (L.skipLineComment  ";;")
  (L.skipBlockComment "#|" "|#")

-- | Turn a parser into one that consumes all whitespace behind it.
lexeme :: P a -> P a
lexeme = L.lexeme sc

-- | Turn a parser of strings into a lexeme of text
lexpack :: P String -> P Text
lexpack = lexeme . fmap pack

-- | Parse a literal symbol and then consume any whitespace
symbol :: Text -> P Text
symbol = L.symbol sc

-- | Turn a parser into one surrounded by parens and lexemed
paren :: P a -> P a
paren = between (symbol "(") (symbol ")")

-- | Parse a string literal demarcated by 2 "'s
strLit :: P Text
strLit = lexpack $ between (char '"') (char '"') (many $ anySingleBut '"')

-- | Parse at least 1 number as an int
num :: P Int
num = lexeme . fmap read $ some (satisfy isNumber)

-- | A word may not start with any prefix-char, and not contain any reserved char
--
-- Note that although the prefix characters *are* legal, they will often cause
-- something to be read incorrectly.
--
-- So:
-- @   = the text '@'
-- @x  = variable dereference of x
-- x@x = the text 'x@x'
--
-- The same with ':', '#', fine on their own or in the middle of text, but at
-- the front will cause the lexeme to be evaluated as something other than text
word :: P Text
word = lexpack $ some (satisfy isWordChar)

-- | Turn two parsers into a parser of a list of pairs
pairs :: P a -> P b -> P [(a, b)]
pairs a b = many $ (,) <$> a <*> b

-- | Match at least 1 of legal keyname characters
keyname :: P Keyname
keyname = word

-- | Match a variety of word-terminators
terminator :: P ()
terminator = choice
  [ void $ satisfy isSpace
  , void $ satisfy isReservedChar
  , eof]

-- | Run the parser IFF it is not followed by a space or eof.
prefix :: P a -> P a
prefix p = try $ p <* notFollowedBy terminator

-- | Sort a list of something that projects into text by ordering it by:
-- * Longest first
-- * On equal length, alphabetically
--
-- This ensures that if want to try a bunch of named parsers, that you won't
-- accidentally match a substring first. E.g.
-- myParser:
--   "app"   -> 1
--   "apple" -> 2
-- >> run myParser "apple"
-- 1
--
-- If the longest strings are always at the top, this problem is automatically
-- avoided.
descendOn :: (a -> Text) -> [a] -> [a]
descendOn f =
  sortBy . (`on` f) $ \a b ->
    case (compare `on` T.length) b a of
      EQ -> compare a b
      x  -> x

-- | Create a parser that matches a single string literal from some collection.
--
-- Longer strings have precedence over shorter ones.
matchOne :: Foldable t => t Text -> P Text
matchOne = choice . map (try . string) . descendOn id . toList

{- SECTION: configurable literals ---------------------------------------------}

{- NOTE: We support *nearly* all flags and options from Configurable. Excluded:
* inescapable: because it can really get you stuck
* dump-keytable: because you really only want to run it once or twice
* cfg-file: because we are already reading a cfg-file
-}

kflagname :: P FlagName
kflagname = lexeme . matchOne $
  [ "verbose", "sections-off", "sections-on", "commands-on", "commands-off"
  , "fallthrough-off", "fallthrough-on" ]

koptname :: P OptionName
koptname = lexeme . matchOne $
  [ "log-level", "key-table", "macro-delay", "compose-key" , "evdev-device-file"
  , "iokit-device-name", "start-delay" , "uinput-device-name", "repeat-rate"
  , "repeat-delay" , "post-init-cmd", "pre-init-cmd"
  ]

kopt :: P (OptionName, Text)
kopt = (,) <$> koptname <*>
  ((word <|> strLit) <?> "word or quoted string")

ksetting :: P KSetting
ksetting = KSetting <$> ((Left <$> kflagname) <|> Right <$> kopt)

{- SECTION: aggregated parsers ------------------------------------------------}

{- SUBSECTION: Blocks ---------------------------------------------------------}

-- | Parse a series of top-level tokens
ktokens :: P KTokens
ktokens = sc *> some kblock <* eof

-- | Parse a top-level token
kblock :: P KBlock
kblock = paren $ choice
  [ try (symbol "defcfg")   >> kcfg
  , try (symbol "defsrc")   >> ksrc
  , try (symbol "defalias") >> kals
  , try (symbol "deflayer") >> klay
  ]

{- ^^ NOTE: It took me a while to figure out, so leaving a note here:

the `try (symbol "name") >> thing` construct is important because:

It will match any of the names with rollback, so, let's say we are going to
apply this parser to a `defalias` block. We first try to match `defcfg`, but
fail on the 4th letter. If we didn't have a `try` in there, the whole parser
would fail and we'd get a parse error. But since we have a try, we roll back
that parser and try the next one. We fail again on `defsrc` but then `defalias`
succeeds, and we proceed to run the `kals` parser on the remaining text.

What I was doing before, however, was running these like this:
`try (symbol "name" >> thing)`.

This will also work, now a failure in name will rollback, and a failure *in the
actual parser* will also roll back. If the defalias block itself was properly
constructed, we'd get the same result, but if we made an error in the block,
we'd get very strange and opaque parse errors. So the former way of structuring
the `try` statements is much better, because it narrows rollback *only to the
pertinent lexemes*. Once we've read a `defalias` symbol, we know that everything
else has to follow the rules of the `klay` parser.

Explaining this now, it seems really obvious, but that is often the way with
these things: only obvious once understood.
-}

-- | Parse a defcfg block by:
kcfg :: P KBlock
kcfg = KCfg <$> many ksetting


-- KCfg <$> pairs (word <?> "setting name") (str <|> word <?> "setting value")

-- | Parse a defsrc block by:
ksrc :: P KBlock
ksrc = KSrc <$> some keyname

-- | Parse a defalias block by:
kals :: P KBlock
kals = KAlias <$> pairs (word <?> "alias name") kexpr

-- | Parse a deflayer block by:
klay :: P KBlock
klay = KLayer <$> word <*> some kexpr

{- SUBSECTION: KExpr ----------------------------------------------------------}

-- | Parse any k-expression
kexpr :: P KExpr
kexpr = choice $
  [ klist
  , ktapmacro
  , kkeylit
  , kderef
  , kkeyword
  , kpause
  , kword -- This must be last, it will match many things
  ]

-- | Parse a parenthesized list as a command followed by a sequence of expressions
klist :: P KExpr
klist = label "parenthesized list" . paren $ do
  cmd  <- word
  args <- some kexpr
  pure $ KList cmd args

-- | Parse a `0x15ae` string as a hex representation of a literal keycode
kkeylit :: P KExpr
kkeylit = label "hexstring" . lexeme $ do
  _ <- prefix $ string "0x"
  s <- some $ satisfy isHexDigit
  pure . KKeyLit . fi $ (read ("0x" <> s) :: Int)

-- | Parse any normal word
kword :: P KExpr
kword = label "legal text" $ KWord <$> word

-- | Parse an @-prefixed word as a dereference
kderef :: P KExpr
kderef = label "@-prefixed variable dereference" $ do
  _ <- prefix $ char '@'
  KDeref <$> word

-- | Parse a `P123` as a pause
kpause :: P KExpr
kpause = label "P-prefixed pause-duration" $ do
  _ <- prefix $ char 'P'
  KPause . fi <$> num

kkeyword :: P KExpr
kkeyword = label ":-prefixed keyword" $ do
  _ <- prefix $ char ':'
  KKeyword <$> word

-- | Parse a #-prefixed list as a tap-macro
ktapmacro :: P KExpr
ktapmacro = label "#-prefixed tap-macro" $ do
  _    <- prefix $ char '#'
  args <- paren $ some kexpr
  pure $ KList "tap-macro" args

{- FIXME: Delete rest-of-file when done ---------------------------------------}

-- | Shorthand for debugging, maybe delete later
--
-- Very handy in the REPL, e.g.
-- >> prs bool "true"
-- True
prs :: P a -> Text -> OnlyIO a
prs p t = case runParser p "" t of
  Left  e -> throwIO $ PErrors e
  Right a -> pure a


tcfg :: OnlyIO ()
tcfg = pp =<< prs (sc >> kcfg) cfg

tsrc :: OnlyIO ()
tsrc = pp =<< prs (sc >> ksrc) src

tals :: OnlyIO ()
tals = pp =<< prs (sc >> kals) als

tfull :: OnlyIO ()
tfull = pp =<< prs ktokens myCfg
