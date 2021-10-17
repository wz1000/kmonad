-- |

module Tinker where

import Prelude


type Keycode = Int

data Stack = Head | Wrap Keycode Stack
