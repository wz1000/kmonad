-- |

module System.Keyboard.Operations where

import Prelude
import System.Keyboard.Types

-- | Shorthand functions
pressOf, releaseOf :: Keycode -> KeySwitch
pressOf   = mkKeySwitch Press
releaseOf = mkKeySwitch Release
-- NOTE: These should probably live somewhere else
