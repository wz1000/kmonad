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

{- SECTION: OS ----------------------------------------------------------------}

-- | Easy way to check if we are on a certain OS
onWindows, onMac, onLinux :: Bool
onWindows = currentOS == Windows
onMac     = currentOS == Mac
onLinux   = currentOS == Linux

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
congruencesForCodeForOS :: (HasKeyTable a, HasKeycode c)
  => OS -> c -> Getter a [KeyCongruence]
congruencesForCodeForOS os c = to $ \a -> case os of
  Linux   -> a^..ks.folded.filtered (\k -> k^.keyLin == Just (c^.keycode.from _Keycode))
  Mac     -> a^..ks.folded.filtered (\k -> k^.keyMac == Just (c^.keycode.from _Keycode))
  Windows -> a^..ks.folded.filtered (\k -> k^.keyMac == Just (c^.keycode.from _Keycode))

-- | Lookup a 'Keycode' for your current OS in a KeyTable
congruencesForCode :: (HasKeycode c, HasKeyTable a) => c -> Getter a [KeyCongruence]
congruencesForCode = congruencesForCodeForOS currentOS

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

-- | Lookup the shifted names for a keycode
--
-- These are the names used to refer to what happens when this button is tapped
-- while shift is held. This muddles the semantics a *little* bit, but this
-- table is also the best place to specify locales, and this is therefore a
-- practical place to keep it all together.
shiftedDictForOS :: HasKeyTable a => OS -> Getter a KeyDict
shiftedDictForOS os = to $ \a -> (foldr go mempty $ a^.ks)
  where
    go k acc = acc <> case (os, k^.shiftedName) of
      (_, Nothing) -> mempty
      (Linux, Just n) ->
        maybe mempty (\c -> M.singleton n (c^._Keycode)) (k^.keyLin)
      (Mac, Just n) ->
        maybe mempty (\c -> M.singleton n (c^._Keycode)) (k^.keyMac)
      (Windows, Just n) ->
        maybe mempty (\c -> M.singleton n (c^._Keycode)) (k^.keyWin)

-- | Like 'shiftedDictForOS' but for the 'currentOS'
shiftedDict :: HasKeyTable a => Getter a KeyDict
shiftedDict = shiftedDictForOS currentOS


{- NOTE: I'm not sure if coding keycode lookup like this caches the dict in a
 thunk or not. If it doesn't then this is 'slow', because we are constructing an
 entire dict every time we lookup a keycode. On the other hand, we only do this
 a few times at init, and after that I think we never touch this. There is an
 opportunity here to improve the code by optimizing, and an opportunity to screw
 up my optimizing prematurely. -}

-- | Lookup the Keycode for a Keyname for a given OS
codeForNameForOS :: HasKeyTable a => OS -> Keyname -> Getter a (Maybe Keycode)
codeForNameForOS os n = keyDictForOS os . at n

-- | Lookup the Keycode for a Keyname for the current OS
codeForName :: HasKeyTable a => Keyname -> Getter a (Maybe Keycode)
codeForName n = keyDict . at n

-- | Lookup the Keycode for a shifted name for a given OS
codeForShiftedForOS :: HasKeyTable a => OS -> Keyname -> Getter a (Maybe Keycode)
codeForShiftedForOS os n = shiftedDictForOS os . at n

-- | Lookup the Keycode for a shifted name for the current OS
codeForShifted :: HasKeyTable a => Keyname -> Getter a (Maybe Keycode)
codeForShifted n = shiftedDict . at n
