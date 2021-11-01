{-# LANGUAGE QuasiQuotes #-}
-- |

module KMonad.App.Main.Discover where

import KMonad.Prelude

import KMonad.App.Locale
import KMonad.App.Logging
import KMonad.App.Types


import System.Keyboard
import System.Keyboard.IO

import qualified RIO.Text as T
import Text.RawString.QQ


{- NOTE: doc ------------------------------------------------------------------}

greeting :: Text
greeting = [r| Welcome to KMonad's key-discovery report.

Please push any key, and we will report what we know about it. The output format
of the line(s) describing keycode names is:
name shifted linux mac windows (description of the key)

where:
- name: portable name that will be recognized in the config-file
- shifted: name used to refer to emitted a shifted version of this key
- linux, mac, windows: evaluate as keycode literals, but are not portable
- description: clarification of what the key is.

A value of '~~' means we have no code or name for that entry

Please note the following:
- If a key is not triggering a discovery report, then the event is not reaching
  KMonad, and we cannot capture it. If under Linux or Mac you can try to see if
  this event shows up under another device.
- The literal keycode will differ between OSes. You can refer to the key by its
  code in your config, but then your config will only work for that 1 OS.
- If the key has a name, that name *should* refer to the same key across all
  platforms. If it doesn't: please file an issue on our github page:
  https://github.com/kmonad/kmonad.git
- KMonad ignores all key-repeat events, therefore these also do not show in
  discover-mode.

Unless you started `discover` with the `--inescapable` flag, escape (according
to the currently loaded table) will exit discover mode. If you did use
`--inescapable`, you're going to have to close this process by closing the
terminal its running in with your mouse, or unplugging the keyboard.

Waiting for you to press a key:
|]

{- NOTE: types ----------------------------------------------------------------}

data DiscoverEnv = DiscoverEnv
  { _deLogEnv    :: LogEnv    -- ^ The logging environment
  , _deLocaleEnv :: LocaleEnv -- ^ The locale environment
  , _deKeyI      :: KeyI      -- ^ The key-input environment
  , _deKeyTable  :: KeyTable  -- ^ The keycode table
  }
makeClassy ''DiscoverEnv

instance HasLogEnv   DiscoverEnv where logEnv   = deLogEnv
instance HasKeyI     DiscoverEnv where keyI     = deKeyI
instance HasKeyTable DiscoverEnv where keyTable = deKeyTable

type D a = RIO DiscoverEnv a

{- SECTION: Exception ---------------------------------------------------------}

data DiscoverException = NoEscapeCode KeyTable
  deriving Show

instance Exception DiscoverException where
  -- NOTE: I'd like to print the currently loaded table, but first we need a
  -- table-to-text export function.
  displayException (NoEscapeCode _) = concat
    [ "Could not find a keycode for 'esc' in the current table. Either "
    , "add a keycode for escape to your table, or pass the `--inescapable` "
    , "to the discover invocation."
    ]

{- SECTION: io -------------------------------------------------------------------}

runDiscover :: CanRoot m env => DiscoverCfg -> m ()
runDiscover cfg = if cfg^.dumpKeyTable then atError $ log tableEnUSText else do

  logenv <- view logEnv
  keytbl <- view keyTable
  esc    <- view $ codeForName "esc"

  -- Set up a function to check if a keycode is equal to escape
  isDone <- case (cfg^.escapeExits, esc) of
    (False, _)     -> pure $ const False
    (True, Just c) -> pure $ \s -> (c ==) . (view keycode) $ (s :: KeySwitch)
    _              -> throwIO (NoEscapeCode keytbl)

  withKeyI (cfg^.inputCfg) $ \ki -> do
    let dscenv = DiscoverEnv
                   { _deLogEnv    = logenv
                   , _deKeyI      = ki
                   , _deLocaleEnv = locenv
                   }
    runRIO dscenv $ do

      atInfo $ sep >> log greeting
      let step = do k <- getKey
                    discoverReport k
                    unless (isDone k) step
      step


discoverReport :: KeySwitch -> D ()
discoverReport s = atError $ do
  sep
  log $ "Type:    " <> tshow (s^.switch)
  log $ "Keycode: " <> tshow (s^.keycode)

  view (namesForCode s) >>= \case
    []  -> log "We have no names on file for this key."
    [k] -> dsp k
    ks  -> do log "We have multiple names on file for this key: "
              mapM_ dsp ks