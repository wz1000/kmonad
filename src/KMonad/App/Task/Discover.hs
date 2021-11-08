{-# LANGUAGE QuasiQuotes #-}
-- |

module KMonad.App.Task.Discover where

import Preface

import KMonad.App.IO
import KMonad.App.CfgFile
import KMonad.App.Cmds
import KMonad.App.Locale
import KMonad.App.Logging
import KMonad.App.Types

import KMonad.Util.Ctx

import System.Keyboard
import System.Keyboard.IO

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

{- SECTION: types -------------------------------------------------------------}

data DiscoverEnv = DiscoverEnv
  { _deDiscoverCfg :: DiscoverCfg -- ^ The configuration passed to discover
  , _deRootEnv     :: RootEnv     -- ^ Copy of the containing RootEnv
  , _deLocaleEnv   :: LocaleEnv   -- ^ The locale environment
  , _deInputEnv        :: InputEnv        -- ^ The key-input environment
  }
makeClassy ''DiscoverEnv

instance HasDiscoverCfg DiscoverEnv where discoverCfg = deDiscoverCfg
instance HasRootEnv     DiscoverEnv where rootEnv     = deRootEnv
instance HasLogEnv      DiscoverEnv where logEnv      = rootEnv.logEnv
instance HasLocaleEnv   DiscoverEnv where localeEnv   = deLocaleEnv
instance HasInputEnv        DiscoverEnv where keyI        = deInputEnv

type D a = RIO DiscoverEnv a

{- SECTION: io ----------------------------------------------------------------}

withDiscover :: (CanRoot m env)
  => (DiscoverEnv -> m a) -> m a
withDiscover = runCtx $ ctxDiscover

-- | Grab the config from the reader monad
--
-- This throws an awful 'error' that should be unreachable. The *only* way it
-- can be triggered is if we call 'ctxCfg' when we are *not* running a Task.
-- Full blown programmer error, shamefur dispray.
ctxCfg :: (CanRoot m env) => Ctx r m DiscoverCfg
ctxCfg = lift $ preview (rootEnv.task._Discover) >>= \case
    Nothing -> error "programmer error"
    Just c_ -> pure c_

-- | Open all the contexts required to run discover
ctxDiscover :: (CanRoot m env)
  => Ctx r m DiscoverEnv
ctxDiscover = do
  -- Extract the config from the reader to see if we should check othe config
  -- file, then do it again after updating the reader with the contents of the
  -- cfg. We must do this twice because 'discover' supports the option of not
  -- reading the config-file.
  c <- ctxCfg
  ctxLocal cfgFile $ if c^.checkConfig then id else const Nothing
  ctxLoadedCfg
  c <- ctxCfg

  locenv <- ctxLocale $ c^.discoverCfg.localeCfg

  ctxBracket (triggerHook PreAcquire) (triggerHook PostRelease)
  ki <- ctxFromWith withInputEnv $ c^.inputCfg
  ctxBracket (triggerHook PostAcquire) (triggerHook PreRelease)

  mkCtx $ \f -> do
    rootenv <- view rootEnv
    f $ DiscoverEnv
      { _deDiscoverCfg = c^.discoverCfg
      , _deRootEnv     = rootenv
      , _deLocaleEnv   = locenv
      , _deInputEnv        = ki }

-- | Open the context and start the listener
runDiscover :: (CanRoot m env) => m ()
runDiscover = withDiscover . inEnv $ do
  cfg <- view discoverCfg
  if cfg^.dumpKeyTable then atError $ log tableEnUSText else do

    atWarn $ sep >> log greeting

    esc <- getRequiredCode "esc"
    let isDone e = (cfg^.escapeExits) && (e^.keycode == esc)
    let step = do k <- getKey
                  discoverReport k
                  unless (isDone k) step

    step

-- | Print some information about a KeySwitch
discoverReport :: KeySwitch -> D ()
discoverReport s = atError $ do
  sep
  log $ "Type:    " <> tshow (s^.switch)
  log $ "Keycode: " <> tshow (s^.keycode)

  view (localeEnv.congruencesForCode s) >>= \case
    []  -> log "We have no names on file for this key."
    [k] -> dsp k
    ks  -> do log "We have multiple names on file for this key: "
              mapM_ dsp ks

{- NOTE: Why Ctx is handy. Here is withDiscover without Ctx
-- | Open all the contexts required to run discover
withDiscover :: (CanRoot m env, HasDiscoverCfg c)
  => c -> (DiscoverEnv -> m a) -> m a
withDiscover c f = do
  locally cfgFile (if c^.checkConfig then id else const Nothing) $
    withLoadedCfg $
      withLocale (c^.discoverCfg.localeCfg) $ \locenv ->
        bracket_ (triggerHook PreAcquire) (triggerHook PostRelease) $
          withKeyI (c^.discoverCfg.inputCfg) $ \ki ->
            bracket_ (triggerHook PostAcquire) (triggerHook PreRelease) $ do
              rootenv <- view rootEnv
              f $ DiscoverEnv
                    { _deDiscoverCfg = c^.discoverCfg
                    , _deRootEnv     = rootenv
                    , _deLocaleEnv   = locenv
                    , _deKeyI        = ki }
-}
