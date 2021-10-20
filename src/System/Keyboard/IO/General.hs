{-# OPTIONS_GHC -Wno-orphans #-}
{- General, cross-platform IO stuff -}


module System.Keyboard.IO.General where

import System.Keyboard.Prelude
import System.Keyboard.Operations
import System.Keyboard.Types

import System.Keyboard.IO.Evdev
import System.Keyboard.IO.Uinput



import RIO.Partial (read, toEnum)
import qualified RIO.Text as T

{- NOTE: Pleasant wrappers ----------------------------------------------------}

getKey :: (MonadIO m, MonadReader env m, HasKeyI env) => m KeySwitch
getKey = view (keyI . uKeyI) >>= liftIO


{- NOTE: KeyTable contexts ----------------------------------------------------}

withKeyTable :: MonadUnliftIO m => KeyTableCfg -> (KeyTable -> m a) -> m a
withKeyTable EnUS              f = f enUSTable
withKeyTable (CustomTable pth) f = f =<< readKeyTable <$> readFileUtf8 pth


{- NOTE: OS-agnostic key-IO ---------------------------------------------------}

withInput :: MonadUnliftIO m => InputCfg -> (KeyI -> m a) -> m a
withInput c f = do
  threadDelay $ 1000 * c^.startDelay
  case c^.inputToken of
    (Evdev c) -> withEvdev c $ \(BasicKeyI i) -> f (KeyI i)

instance CanOpenKeyI InputCfg where withKeyI i = withInput i
