{- NOTE: -Wno-orphans because we need to define DefaultKeytable here -}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module System.Keyboard.Operations where

import System.Keyboard.Prelude
import System.Keyboard.EnUS
import System.Keyboard.Types

import RIO.Partial (read, toEnum)
import qualified RIO.HashMap as M
import qualified RIO.Text as T

-- | Shorthand functions
pressOf, releaseOf :: Keycode -> KeySwitch
pressOf   = mkKeySwitch Press
releaseOf = mkKeySwitch Release
-- NOTE: These should probably live somewhere else as they are smart construtcors


{- SECTION: Keycodes ----------------------------------------------------------}

-- | Read a 'Keycode' from Text.
readKeycode :: IsKeycode c => Text -> Maybe c
readKeycode "" = Nothing
readKeycode t  = Just . view (from _Keycode) $ toEnum (read (unpack t) :: Int)

{- SECTION: KeyTables ---------------------------------------------------------}

{- SUBSECTION: Reading from text ----------------------------------------------}

-- | Read a 'KeyTable' from Text.
readKeyTable :: Text -> KeyTable
readKeyTable = KeyTable . map g . drop 2 . T.lines
  where
    g t = let [n, s, l, m, w, d] = map T.strip
                                 . take 6 . drop 1
                                 . T.split (== '|') $ t
          in KeyCongruence
             { _keyName        = n
             , _shiftedName    = readShifted s
             , _keyDescription = d
             , _keyLin         = readKeycode l
             , _keyMac         = readKeycode m
             , _keyWin         = readKeycode w
             }
    readShifted "" = Nothing
    readShifted s  = Just s

-- | The builtin `standard` 'KeyTable'
tableEnUS :: KeyTable
tableEnUS = readKeyTable tableEnUSText

instance Default KeyTable where def = tableEnUS

{- SUBSECTION: looking up stuff -----------------------------------------------}

-- | Get at the list of entries for anything that has a key table
ks :: HasKeyTable a => Getter a [KeyCongruence]
ks = keyTable . uKeyTable

-- | Lookup a Keycode for a particular OS in a KeyTable
--
-- Note that there may be 0, 1, or multiple entries for a given (OS, Keycode)
-- tuple, since it is possible to bind many names to the same key.
namesForCodeForOS :: (HasKeyTable a, HasKeycode c)
  => OS -> c -> Getter a [KeyCongruence]
namesForCodeForOS os c = to $ \a -> case os of
  Linux   -> a^..ks.folded.filtered (\k -> k^.keyLin == Just (c^.keycode.from _Keycode))
  Mac     -> a^..ks.folded.filtered (\k -> k^.keyMac == Just (c^.keycode.from _Keycode))
  Windows -> a^..ks.folded.filtered (\k -> k^.keyMac == Just (c^.keycode.from _Keycode))

-- | Lookup a 'Keycode' for your current OS in a KeyTable
namesForCode :: (HasKeycode c, HasKeyTable a) => c -> Getter a [KeyCongruence]
namesForCode = namesForCodeForOS currentOS

-- | Look up the KeyDict for a specific OS
keyDictForOS :: HasKeyTable a => OS -> Getter a KeyDict
keyDictForOS os = to $ \a -> (foldr go mempty $ a^.ks)
  where
    go k acc = acc <> case os of
      Linux   ->
        maybe mempty (\c -> M.singleton (k^.keyName) (c^._Keycode)) (k^.keyLin)
      Mac     ->
        maybe mempty (\c -> M.singleton (k^.keyName) (c^._Keycode)) (k^.keyMac)
      Windows ->
        maybe mempty (\c -> M.singleton (k^.keyName) (c^._Keycode)) (k^.keyWin)

-- | Look up the KeyDict for the current OS
keyDict :: HasKeyTable a => Getter a KeyDict
keyDict = keyDictForOS currentOS

{- NOTE: I'm not sure if coding keycode lookup like this caches the dict in a
 thunk or not. If it doesn't then this is 'slow', because we are constructing an
 entire dict every time we lookup a keycode. On the other hand, we only do this
 a few times at init, and after that I think we never touch this. There is an
 opportunity here to improve the code by optimizing, and an opportunity to screw
 up my optimizing prematurely. -}

-- | Lookup the Keycode for a Keyname for a given OS
codeForNameForOS :: HasKeyTable a => OS -> Keyname -> Getter a (Maybe Keycode)
codeForNameForOS os n = keyDictForOS os . at n

-- | Lookup the Keycode for a Keyname for a given OS
codeForName :: HasKeyTable a => Keyname -> Getter a (Maybe Keycode)
codeForName n = keyDict . at n
