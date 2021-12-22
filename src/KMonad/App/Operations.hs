-- |

module KMonad.App.Operations where

import Preface
import KMonad.Util.Logging
import KMonad.App.Types


{- NOTE: Should this live here? Maybe in App.IO?

Anyways: first make it work, then make it good.

-}

-- | Run a RIO BasicEnv in IO
-- runBasic :: RootCfg -> RIO BasicEnv a -> OnlyIO a
-- runBasic c a = do

--   let logcfg = (def :: LogCfg)
--         & logLvl .~ (c^.logLevel)
--         & logSep .~ (if c^.logSections then line else noSep)
--   withLogging logcfg $ \logenv -> runRIO logenv $ do

--     withKeyTable (c^.keyTableCfg) $ \keytbl -> do

--       let basenv = BasicEnv
--                 { _geRootCfg   = c
--                 , _geLogEnv     = logenv
--                 , _geKeyTable   = keytbl
--                 }
--       runRIO basenv a
