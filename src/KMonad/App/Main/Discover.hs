{-# LANGUAGE QuasiQuotes #-}
-- |

module KMonad.App.Main.Discover where

import KMonad.Prelude

import KMonad.App.Types
import KMonad.Util.Logging

import System.Keyboard
import System.Keyboard.IO

import qualified RIO.Text as T
import Text.RawString.QQ

{- NOTE: doc ------------------------------------------------------------------}

greeting :: Text
greeting = [r| Welcome to KMonad's key-discovery report.

Please push any key, and we will report what we know about it. The output format
of the line(s) describing keycode names is:
name linux mac windows (description of the key)

where:
- name portable name that will be recognized in the config-file
- linux, mac, windows: evaluate as keycode literals, but are not portable
- description: clarification of what the key is.

A keycode literal of '~' means we have no code for the name on that OS

Please note the following:
- If a key is not triggering a discovery report, then the event is not reaching
  KMonad, an we cannot capture it. If under Linux or Mac you can try to see if
  this event shows up under another device.
- The literal keycode will differ between OSes. You can refer to the key by its
  code in your config, but then your config will only work for that 1 OS.
- If the key has a name, that name *should* refer to the same key across all
  platforms. If it doesn't: please file an issue on out github page:
  https://github.com/kmonad/kmonad.git
- We ignore all key-repeat events, these also do not show in discover-mode.

Waiting for you to press a key:
|]

{- NOTE: types ----------------------------------------------------------------}

data DiscEnv = DiscEnv
  { _deLog  :: LogEnv       -- ^ The logging environment
  , _deKeyI :: KeyI         -- ^ The key-input environment
  , _deKeyTable :: KeyTable -- ^ The keycode table
  }
makeClassy ''DiscEnv

instance HasLogEnv   DiscEnv where logEnv   = deLog
instance HasKeyI     DiscEnv where keyI     = deKeyI
instance HasKeyTable DiscEnv where keyTable = deKeyTable

type D a = RIO DiscEnv a

{- NOTE: io -------------------------------------------------------------------}

runDiscover :: CanG m env => DiscoverCfg -> m ()
runDiscover cfg = if cfg^.dumpEnUS then logError enUSTableText else do
  logenv <- view logEnv
  keytbl <- view keyTable
  withKeyI (cfg^.inputCfg) $ \ki -> do

    let dscenv = DiscEnv
                   { _deLog      = logenv
                   , _deKeyI     = ki
                   , _deKeyTable = keytbl
                   }
    runRIO dscenv $ do
      sepInfo >> logInfo greeting
      forever (getKey >>= discoverReport)

discoverReport :: KeySwitch -> D ()
discoverReport s = do
  sepError
  logError $ "Type:    " <> tshow (s^.switch)
  logError $ "Keycode: " <> tshow (s^.keycode)

  getNames (s^.keycode) >>= \case
    []  -> logError "We have no names on file for this key."
    [k] -> dspError k
    ks  -> do logError "We have multiple names on file for this key: "
              mapM_ dspError ks
