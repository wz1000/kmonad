-- |

module Edifice.Ctx.Types where

import Util

import Art
import Edifice.Invoc







data Dais = Dais
  { _dInvoc  :: Invoc
  , _dLogEnv :: LogEnv
  }
makeClassy ''Dais

instance HasLogEnv Dais where logEnv = dLogEnv
instance HasInvoc Dais where invoc = dInvoc

type D a = RIO Dais a
-- type OnDais r m = (MonadReader r m, MonadUnliftIO m, HasDais r)

-- type IOCtx r m = (MonadReader r m, MonadUnliftIO m)
-- type InvokedCtx r m = (IOCtx r m, HasLogEnv r)
-- type CmdsCtx    r m = (IOCtx r m, HasCmdsEnv r)
-- type BaseCtx    r m = (InvokedCtx r m, CmdsCtx r m)
