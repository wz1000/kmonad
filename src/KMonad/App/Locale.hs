-- |

module KMonad.App.Locale where

import KMonad.Prelude
import System.Keyboard
import System.Keyboard.IO


{- SECTION: Types -------------------------------------------------------------}

-- | Configuration settings that specify keycode/keyname mappings
data LocaleCfg = LocaleCfg
  { _keyTableCfg :: !KeyTableCfg -- ^ Keycode/keyname correspondence settings
  , _composeKey  :: !Keyname     -- ^ Keyname of keycode to use as a compose-key
  , _stdShift    :: !Keyname     -- ^ Keyname of keycode to use as shift for shifted buttons
  } deriving (Eq, Show)
makeClassy ''LocaleCfg

instance Default LocaleCfg where def = LocaleCfg EnUS "cmps" "lsft"

-- | Runtime environment for locale computations
data LocaleEnv = LocaleEnv
  { _lLocaleCfg   :: !LocaleCfg -- ^ Reference to the LocaleCfg
  , _composeCode  :: !Keycode
  , _stdShiftCode :: !Keycode
  , _lKeyTable    :: !KeyTable  -- ^ Loaded 'KeyTable'
  }
makeClassy ''LocaleEnv

instance HasKeyTable LocaleEnv where keyTable = lKeyTable

type CanLocale m env = (MonadReader env m, HasLocaleEnv env)

-- | Things that can go wrong with locale computations
data LocaleException
  = UnknownComposeKey  Keyname -- ^ Compose key does not occur in keytable
  | UnknownStdShiftKey Keyname -- ^ Shift key does not occur in keytable
  deriving (Eq, Show)

instance Exception LocaleException where
  displayException (UnknownComposeKey n)
    = "Compose key not in KeyTable: "  <> unpack n
  displayException (UnknownStdShiftKey n)
    = "StdShift key not in KeyTable: " <> unpack n

{- SECTION: IO ----------------------------------------------------------------}

-- | Run a function in the context of an acquired locale
--
-- Note, this will throw an exception if the compose-key or standard-shift-key
-- do not occur in the keytable.
withLocale :: (IO m, HasLocaleCfg c) => c -> (LocaleEnv -> m a) -> m a
withLocale c f = withKeyTable (c^.keyTableCfg) $ \tbl -> do
  cc <- case tbl^.keyDict.at (c^.composeKey) of
          Nothing -> throwIO $ UnknownComposeKey (c^.composeKey)
          Just c  -> pure c
  sc <- case tbl^.keyDict.at (c^.stdShift) of
          Nothing -> throwIO $ UnknownStdShiftKey (c^.stdShift)
          Just c  -> pure c
  f $ LocaleEnv (c^.localeCfg) cc sc tbl
