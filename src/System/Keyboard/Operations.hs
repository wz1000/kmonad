-- |

module System.Keyboard.Operations where

import System.Keyboard.Prelude
import System.Keyboard.EnUS
import System.Keyboard.Types

import RIO.Partial (read, toEnum)
import qualified RIO.Text as T

-- | Shorthand functions
pressOf, releaseOf :: Keycode -> KeySwitch
pressOf   = mkKeySwitch Press
releaseOf = mkKeySwitch Release
-- NOTE: These should probably live somewhere else as they are smart construtcors

{- NOTE: Parsing text as keycodes ---------------------------------------------}

-- | Read a 'Keycode' from Text.
readKeycode :: IsKeycode c => Text -> Maybe c
readKeycode "~" = Nothing
readKeycode t   = Just . view (from _Keycode) $ toEnum (read (unpack t) :: Int)

-- | Read a 'KeyTable' from Text.
readKeyTable :: Text -> KeyTable
readKeyTable = KeyTable . map g . drop 2 . T.lines
  where g t = let [n, l, m, w, d] = map T.strip
                                . take 5 . drop 1
                                . T.split (== '|') $ t
              in KeyCongruence n d (readKeycode l) (readKeycode m) (readKeycode w)

-- | The builtin `standard` 'KeyTable'
enUSTable :: KeyTable
enUSTable = readKeyTable enUSTableText


{- NOTE: simple KeyTable operations -------------------------------------------}

-- | Lookup a Keycode for a particular OS in a KeyTable
--
-- Note that there may be 0, 1, or multiple entries for a given (OS, Keycode)
-- tuple, since it is possible to bind many names to the same key.
lookupKeycode' :: KeyTable -> (OS, Keycode) -> [KeyCongruence]
lookupKeycode' (KeyTable ks) (os, c) = case os of
  Linux   -> ks^..folded.filtered (\k -> k^.keyLin == Just (c^.from _Keycode))
  Mac     -> ks^..folded.filtered (\k -> k^.keyMac == Just (c^.from _Keycode))
  Windows -> ks^..folded.filtered (\k -> k^.keyMac == Just (c^.from _Keycode))

-- | Lookup a 'Keycode' for your current OS in a KeyTable
lookupKeycode :: KeyTable -> Keycode -> [KeyCongruence]
lookupKeycode t = lookupKeycode' t . (currentOS, )

-- | Lookup a full 'KeyCongruence' record for a 'Keyname'
--
-- Note that there can be only 0 or 1 entries for a given 'Keyname', as
-- 'Keyname's in a 'KeyTable' are guaranteed to be unique.
lookupKeyname' :: KeyTable -> Keyname -> Maybe KeyCongruence
lookupKeyname' (KeyTable ks) n = ks^?folded.filtered (\k -> k^.keyName == n)

-- | Lookup the 'Keycode' for a 'Keyname' under the current OS
lookupKeyname :: KeyTable -> Keyname -> Maybe Keycode
lookupKeyname t n = grab =<< lookupKeyname' t n
  where grab k = case currentOS of Linux   -> k^?keyLin._Just._Keycode
                                   Mac     -> k^?keyMac._Just._Keycode
                                   Windows -> k^?keyWin._Just._Keycode

{- NOTE: monadic KeyTable operations for ease of use --------------------------}

type CanKeyTable m env = (MonadReader env m, HasKeyTable env)

-- | Lookup all table entries for the 'Keycode' under the current OS
getNames :: CanKeyTable m env => Keycode -> m [KeyCongruence]
getNames c = (`lookupKeycode` c) <$> view keyTable

-- | Lookup a 'Keycode' for some 'Keyname' under the current OS
getCode :: CanKeyTable m env => Keyname -> m (Maybe Keycode)
getCode n = (`lookupKeyname` n) <$> view keyTable
