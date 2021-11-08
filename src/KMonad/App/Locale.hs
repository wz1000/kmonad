-- |

module KMonad.App.Locale where

import Preface
import KMonad.Util.Ctx

import System.Keyboard
import System.Keyboard.IO

import RIO.Partial (fromJust)
import qualified RIO.HashMap as M
import qualified RIO.Set     as S


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
  | MissingKey         Keyname -- ^ Required key does not occur in keytable
  deriving (Eq, Show)

instance Exception LocaleException where
  displayException (UnknownComposeKey n)
    = "Compose key not in KeyTable: "  <> unpack n
  displayException (UnknownStdShiftKey n)
    = "StdShift key not in KeyTable: " <> unpack n
  displayException (MissingKey n)
    = "KeyTable is missing required key: " <> unpack n

-- | Keynames that *must* occur in the table because we reference them in the code
requiredKeynames :: S.Set Keyname
requiredKeynames = S.fromList $
  [ "esc", "lctl", "rctl", "lalt", "ralt", "lsft", "rsft", "lmet", "rmet" ]

{- SECTION: Operations --------------------------------------------------------}

-- | Get a required keycode, which is guaranteed to exist
-- (outside of programmer error)
getRequiredCode :: CanLocale m env => Keyname -> m Keycode
getRequiredCode = maybe err pure <=< getCode
  where err = error "Programmer error: trying to access wrong required keyname"

-- | Return the code for some keyname on the current OS
getCode :: CanLocale m env => Keyname -> m (Maybe Keycode)
getCode n = view (localeEnv.codeForName n)

{- SECTION: IO ----------------------------------------------------------------}

-- | Run a function in the context of an acquired locale
--
-- Note, this will throw an exception if the compose-key or standard-shift-key
-- do not occur in the keytable, or if the keytable is missing any of the
-- 'requiredKeynames'.
withLocale :: (IO m, HasLocaleCfg c) => c -> (LocaleEnv -> m a) -> m a
withLocale c f = withKeyTable (c^.keyTableCfg) $ \tbl -> do
  let d = tbl^.keyDict
  cc <- case d^.at (c^.composeKey) of
          Nothing -> throwIO $ UnknownComposeKey (c^.composeKey)
          Just c  -> pure c
  sc <- case d^.at (c^.stdShift) of
          Nothing -> throwIO $ UnknownStdShiftKey (c^.stdShift)
          Just c  -> pure c
  forM_ requiredKeynames $
    \req -> when (not $ M.member req d) $ throwIO (MissingKey req)
  f $ LocaleEnv (c^.localeCfg) cc sc tbl

-- ctxLocale :: (IO m, HasLocaleCfg c) => c -> Ctx r m LocaleEnv
-- ctxLocale c = mkCtx $ \f -> withLocale c f

ctxLocale :: (IO m, HasLocaleCfg c) => c -> Ctx r m LocaleEnv
ctxLocale = ctxFromWith withLocale
