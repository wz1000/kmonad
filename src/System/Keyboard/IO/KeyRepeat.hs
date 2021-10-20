-- |

module System.Keyboard.IO.KeyRepeat where


import System.Keyboard.Prelude

import System.Keyboard.Types

withKeyRepeat :: MonadUnliftIO m => KeyRepeatCfg -> KeyO -> (KeyRepeatEnv -> m a) -> m a
withKeyRepeat = undefined
