-- |

module KMonad.App.Main.Discover where

import KMonad.Prelude

import KMonad.App.Types
import KMonad.Util.Logging

import System.Keyboard
import System.Keyboard.IO

import qualified RIO.Text as T

data DiscEnv = DiscEnv
  { _deLog :: LogEnv
  , _deKeyI :: KeyI
  }
makeClassy ''DiscEnv

instance HasLogEnv DiscEnv where logEnv = deLog
instance HasLogCfg DiscEnv where logCfg = deLog.logCfg
instance HasKeyI   DiscEnv where keyI   = deKeyI

myTable :: OnlyIO KeyTable
myTable = loadKeyTable "/home/david/prj/kmonad/keycode_table/en_US.md"

runDiscover :: OnlyLIO ()
runDiscover = do
  tbl <- loadKeyTable "/home/david/prj/kmonad/keycode_table/en_US.md"
  runDiscover' (EvdevCfg "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd") tbl

runDiscover' :: (CanOpenKeyI icfg) => icfg -> KeyTable -> OnlyLIO ()
runDiscover' icfg tbl = do

  logError $ T.unlines
    [ "\nWelcome to KMonad's key-discovery report."
    , "Please push any key, and we will report what we know about it.\n"
    , "The output format of the line(s) describing keycode names is:"
    , "name linux mac windows (description of the key)"
    , "where:"
    , "- name portable name that will be recognized in the config-file"
    , "- linux, mac, windows: evaluate as keycode literals, but are not portable"
    , "- description: clarification of what the key is."
    , "A keycode literal of '~' means we have no code for the name on that OS"
    , "\nPlease note the following:"
    , "- If a key is not triggering a discovery report, then the event"
    , "  is not reaching KMonad, an we cannot capture it. If under Linux or Mac"
    , "  you can try to see if this event shows up under another device."
    , "- The literal keycode will differ between OSes. You can refer to"
    , "  the key by its code in your config, but then your config will only"
    , "  work for that 1 OS."
    , "- If the key has a name, that name *should* refer to the same key"
    , "  across all platforms. If it doesn't: please file an issue on "
    , "  out github page: https://github.com/kmonad/kmonad.git"
    , "- We ignore all key-repeat events, these also do not show in"
    , "  discover-mode."
    , "\nWaiting for you to press a key:"
    ]

  le <- view logEnv
  withKeyI icfg $ \ki -> do

    let env = DiscEnv le ki
    runRIO env (forever $ getKey >>= discoverReport tbl)

-- | Prettyprint a KeyCongruence
ppKC :: KeyCongruence -> Text
ppKC k = mconcat
  [ k^.keyName, " "
  , maybe "~" tshow (k^?keyLin._Just._Keycode), " "
  , maybe "~" tshow (k^?keyMac._Just._Keycode), " "
  , maybe "~" tshow (k^?keyWin._Just._Keycode), " "
  , "(", k^.keyDescription, ")"
  ]

discoverReport :: LIO m env => KeyTable -> KeySwitch -> m ()
discoverReport t s = do

  logError $ "\n-- Received event: ---------------------------------"
  logError $ "Type:    " <> tshow (s^.switch)
  logError $ "Keycode: " <> tshow (s^.keycode)

  case lookupKeycode t (s^.keycode) of
    []  -> logError "We have no names on file for this key."
    [k] -> logError . ppKC $ k
    ks  -> do logError "We have multiple names on file for this key: "
              mapM_ (logError . ppKC) ks
