-- |

module Evdev.Operations where

import Preface

import System.Keyboard
import qualified RIO.Text as T
import RIO.Text.Partial (replace)
import Text.Megaparsec
import Text.Megaparsec.Char


class Syntax s e a | s -> e where
  toSyntax   :: s -> a    -> Text
  fromSyntax :: s -> Text -> Either e a

_Syntax :: Syntax s e a => s -> Prism' Text a
_Syntax s = prism' (toSyntax s)
  $ either (const Nothing) Just . fromSyntax s

data KeySyntax = KeySyntax
  { _pressChar   :: Char
  , _releaseChar :: Char
  }
makeLenses ''KeySyntax

instance Default KeySyntax where
  def = KeySyntax '<' '>'

data KeySyntaxError = KeycodeParseError

encodeKey :: KeySyntax -> KeySwitch -> Text
encodeKey x s = let c = T.singleton $ bool (x^.releaseChar) (x^.pressChar) $ s^.isPress
  in tshow (s^.keycode.from _Keycode :: Word64) <> c

encodeKeys :: KeySyntax -> [KeySwitch] -> Text
encodeKeys x = foldMap (encodeKey x)

decodeKeys :: KeySyntax -> Text -> Either KeySyntaxError [KeySwitch]
decodeKeys = undefined

instance Syntax KeySyntax KeySyntaxError [KeySwitch] where
  toSyntax = undefined
  fromSyntax = decodeKeys

testSeq :: [KeySwitch]
testSeq = [pressOf 1, pressOf 2, releaseOf 2, releaseOf 1]

-- instance Syntax KeySwitch where
