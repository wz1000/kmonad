-- |

module System.Keyboard.Operations where

import System.Keyboard.Prelude
import System.Keyboard.Types

-- | Shorthand functions
pressOf, releaseOf :: Keycode -> KeySwitch
pressOf   = mkKeySwitch Press
releaseOf = mkKeySwitch Release
-- NOTE: These should probably live somewhere else

{- NOTE: KeyTable operations --------------------------------------------------}


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


