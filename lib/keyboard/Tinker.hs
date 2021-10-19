-- |

module Tinker where

import Prelude

{- NOTE: old stuff ------------------------------------------------------------}

type Keycode = Int

data Stack = Head | Wrap Keycode Stack
