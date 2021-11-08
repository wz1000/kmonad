{- For helpers that are so ubiquitous I want them everywhere -}
module Preface.Util
  ( u
  , ppIO
  , fi
  , inEnv
  , overMVar
  , reverseMap
  , duplicates

  , untilJust
  , ifM

  , nil
  , uncurry3
  , singleton

  , showHex
  )
where

import Preface.Imports
import Preface.Types

import Text.Pretty.Simple (pPrint)

import qualified RIO.HashMap as M
import qualified RIO.Set     as S
import qualified Numeric     as N (showHex)

--------------------------------------------------------------------------------
-- $uncategorized


-- | undefined is too long to type when debugging
u :: a
u = undefined

-- | useful to pretty-print a record when debugging
ppIO :: (MonadIO m, Show a) => a -> m ()
ppIO = pPrint

-- | Shorthand for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Makes some of the continuation formulations cleaner to write
inEnv :: IO m => RIO env a -> env -> m a
inEnv = flip runRIO

-- | Slightly different modifyMVar to make 1-liners cleaner
overMVar :: UIO m =>  m (MVar a) -> (a -> m (a, b))  -> m b
overMVar a f = a >>= \mv -> modifyMVar mv f

-- | Reverse a map
--
-- NOTE: This might overwrite duplicates.
reverseMap :: (Eq a, Eq b, Hashable a, Hashable b)
  => M.HashMap a b -> M.HashMap b a
reverseMap = M.fromList . toListOf (folded . swapped) . M.toList

-- | Return a set of entries that occur at least more than once
duplicates :: (Foldable t, Ord a) => t a -> S.Set a
duplicates as = snd $ foldl' go (S.empty, S.empty) as where
  go (seen, res) a | a `S.member` seen = (seen, S.insert a res)
                   | otherwise         = (S.insert a seen, res)

--------------------------------------------------------------------------------
-- $maybe-flow
--
-- Easier flow-control using Maybe values

-- | Run a monadic action until a Just occurs
untilJust :: Monad m => m (Maybe a) -> m a
untilJust go = go >>= \case
  Nothing -> untilJust go
  Just a  -> pure a

-- | Choose a branch based on the result of some monadic bool
ifM :: Monad m
  => m Bool -- ^ Test to run
  -> m a    -- ^ Branch on False
  -> m a    -- ^ Branch on True
  -> m a    -- ^ result
ifM mb a b = mb >>= bool a b

--------------------------------------------------------------------------------

-- Used so often it warrants a shorthand
nil :: Applicative m => m ()
nil = pure ()

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

singleton :: a -> [a]
singleton = (:[])

-- | Show an integral as the hex-string that would construct it.
showHex :: (Integral a, Show a) => a -> String
showHex = ("0x" <>) . ($ "") . N.showHex
