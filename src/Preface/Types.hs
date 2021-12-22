{-# OPTIONS_GHC -Wno-orphans #-}
{- Additionaly types that should always be available -}
module Preface.Types
  ( Default(..)

  , Name
  , HasName(..)
  , Named
  , Description
  , HasDescription(..)

  , CfgFile

  -- * Shorthand
  , OnlyIO, IO, UIO, EnvIO, EnvUIO
  )
where

import Preface.External
import qualified RIO as R
import qualified RIO.HashMap as M

--------------------------------------------------------------------------------

instance Display Natural where textDisplay = tshow

{- SECTION: Informative text aliases ------------------------------------------}

-- | A name for some thing
--
-- Note that, internally, names are subject to being compared. It is up to the
-- programmer to ensure that names are meaningful and comparable.
type Name = Text

class HasName a where name :: Lens' a Name

instance HasName Name where name = id

-- | A map of names to things
type Named a = M.HashMap Name a

-- | A description of some thing
--
-- Note that, internally, descriptions are only ever going to be displayed, and
-- not compared or searched or set.
type Description = Text

class HasDescription a where description :: Getter a Description

instance HasDescription Description where description = id

-- | A type indicating a FilePath that is expected to follow some cfg syntax
type CfgFile = FilePath

{- SECTION: Type shorthand ----------------------------------------------------}

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
