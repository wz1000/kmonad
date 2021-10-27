{-# LANGUAGE QuasiQuotes #-}
-- |
module KMonad.App.Parser.Tokenizer where


import KMonad.Prelude hiding (try)

import KMonad.Util.Time
import System.Keyboard

import KMonad.App.Parser.Test -- FIXME: Remove when done

import Data.Char

import Text.RawString.QQ
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import RIO.Partial (read)

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
tfull = pp =<< prs kfull myCfg

tboop :: OnlyIO ()
tboop = pp =<< prs kboop myCfg


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

-- | Top level config file expressions
data KTop
  = KCfg   [(Text, Text)]  -- ^ A defcfg block with a list of pairs of text
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
str :: P Text
str = lexpack $ between (char '"') (char '"') (many $ anySingleBut '"')

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

{- SECTION: aggregated parsers ------------------------------------------------}

{- SUBSECTION: Top level ------------------------------------------------------}

kfull :: P [KTop]
kfull = do
  _  <- sc
  ks <- some ktop
  _  <- eof
  pure ks

ktop :: P KTop
ktop = paren $ choice
  [ try (symbol "defcfg")   >> kcfg
  , try (symbol "defsrc")   >> ksrc
  , try (symbol "defalias") >> kals
  , try (symbol "deflayer") >> klay
  ]
  -- <?> "defsrc, defcfg, defalias, or deflayer block"

kboop :: P [KTop]
kboop = do
  _ <- sc
  a <- kcfg
  b <- ksrc
  c <- kals
  d <- klay
  e <- klay
  pure [a, b, c, d, e]

-- | Parse a defcfg block by:
kcfg :: P KTop
kcfg = KCfg <$> pairs (word <?> "setting name") (str <|> word <?> "setting value")

-- | Parse a defsrc block by:
ksrc :: P KTop
ksrc = KSrc <$> some keyname

-- | Parse a defalias block by:
kals :: P KTop
kals = KAlias <$> pairs (word <?> "alias name") kexpr

-- | Parse a deflayer block by:
klay :: P KTop
klay = KLayer <$> word <*> some kexpr

{- SUBSECTION: KExpr ----------------------------------------------------------}

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
