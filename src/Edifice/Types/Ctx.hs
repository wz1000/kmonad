-- |

module Edifice.Types.Ctx where

import Util



-- {- SECTION: BaseCtx -----------------------------------------------------------}

-- -- | Configuration options for the base context
-- data BaseCfg = BaseCfg
--   { _bLogCfg  :: LogCfg         -- ^ Logging cfg
--   , _bCmdsCfg :: CmdsCfg        -- ^ Hooks and commands cfg
--   , _cfgFile  :: Maybe FilePath -- ^ Where to look for a config file
--   } deriving (Eq, Show)
-- makeClassy ''BaseCfg

-- instance Default BaseCfg where
--   def = BaseCfg
--     { _bLogCfg  = def
--     , _bCmdsCfg = def
--     , _cfgFile  = Nothing
--     }

-- instance HasLogCfg  BaseCfg where logCfg  = bLogCfg
-- instance HasCmdsCfg BaseCfg where cmdsCfg = bCmdsCfg

-- {- SECTION: ModelCfg ----------------------------------------------------------}

-- -- | Config options pertinent to running a KMonad model
-- data ModelCfg = ModelCfg
--   { _fallthrough :: Bool   -- ^ Whether to rethrow uncaught key events
--   , _macroDelay  :: Ms     -- ^ How long to pause between macro-taps
--   } deriving (Eq, Show)
-- makeClassy ''ModelCfg

-- instance Default ModelCfg where def = ModelCfg True 10





-- data InputCfg = InputCfg deriving (Eq, Show)
-- data OutputCfg = OutputCfg deriving (Eq, Show)
-- data LocaleCfg = LocaleCfg deriving (Eq, Show)
