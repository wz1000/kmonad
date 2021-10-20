-- |
module System.Keyboard.Util.FFI
  ( ReturnCode
  , onNonZero
  , onNonZeroThrow
  )
where

import System.Keyboard.Prelude

import RIO (MonadUnliftIO)

import Control.Exception.Lens
import System.IO.Error.Lens

--------------------------------------------------------------------------------

type ReturnCode = Int

-- | Helper function to throw some haskell-error when an FFI call returns non-zero.
--
-- >>> ffiCall `onNonZero` \n -> throwing _Nope ("The snozzle did not jig the glorp", n)
--
onNonZero :: (Monad m, Integral i)
  => m i
  -> (ReturnCode -> m ())
  -> m ()
onNonZero a f = a >>= \case
  0 -> pure ()
  n -> f . fi $ n

-- | Helper function to throw some error on non-zero
--
-- >>> ffiCall `onNonZeroThrow` _Nope "The snozzle did not jig the glorp"
--
onNonZeroThrow :: (Monad m, Integral i)
  => m i
  -> (AReview SomeException (a, ReturnCode), a)
  -> m ()
onNonZeroThrow a (p, b) = a >>= \case
  0 -> pure ()
  n -> throwing p (b, fi n)
