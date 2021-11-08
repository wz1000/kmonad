{-# OPTIONS_GHC -Wno-orphans #-}
{- Additionaly types that should always be available -}
module Preface.Types
  ( Default(..)

  -- * Shorthand
  , OnlyIO, IO, UIO, EnvIO, EnvUIO
  )
where

import Preface.Imports
import qualified RIO as R

--------------------------------------------------------------------------------

instance Display Natural where textDisplay = tshow

--------------------------------------------------------------------------------
-- $shorthand

-- | Type we use instead of 'IO'
--
-- I realize this might be confusing, but the hard 'RIO.IO' type is used so
-- infrequenty that I want it to really stand with a longer name. On the other
-- hand, 'MonadIO' constraints are ubiquitous throughout the code, so a short
-- name seems appropriate.
--
-- Finally, this also establishes the sequence of OnlyIO -> OnlyLIO (see
-- "KMonad.App.Logging")
type OnlyIO a = R.IO a

-- | Shorthand for MonadIO constraint
type IO m = MonadIO m

-- | Shorthand for MonadUnliftIO
type UIO m = MonadUnliftIO m

-- | Shorthand for MonadIO and MonadReader
type EnvIO m e = (IO m, MonadReader e m)

-- | Shorthand for MonadUnliftIO and MonadReader
type EnvUIO m e = (UIO m, MonadReader e m)
