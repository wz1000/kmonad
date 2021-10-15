{-# LANGUAGE CPP #-}
module System.Keyboard.IO.SendKey
  ( SendKeyEnv
  , CanSendKey
  , sendKeyWrite
  , withSendKey
  )
where

import Prelude

import System.Keyboard.Types

import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable


{- NOTE: Types -----------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | The environment used to handle output operations
newtype SendKeyEnv = SendKeyEnv { _ptr :: Ptr WinPacket }
makeClassy ''SendKeyEnv

-- | A constraint synonym for the required context to run the low-level API
type CanSendKey  m env = (MonadIO m, MonadReader env m, HasSendKeyEnv env)

{- NOTE: FFI -------------------------------------------------------------------
-------------------------------------------------------------------------------}

#if defined mingw32_HOST_OS

foreign import ccall "sendKey" c_sendKey :: Ptr WinPacket -> IO ()

#else

c_sendKey :: Ptr WinPacket -> IO ()
c_sendKey _ = throwIO $ FFIWrongOS "access the SendKey system call" "windows"

#endif

{- NOTE: internal --------------------------------------------------------------
-------------------------------------------------------------------------------}

openSendKey :: IO SendKeyEnv
openSendKey = SendKeyEnv <$> (mallocBytes $ sizeOf (undefined :: WinPacket))

closeSendKey :: SendKeyEnv -> IO ()
closeSendKey = free . view ptr


{- NOTE: API -------------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | Write a 'KeySwitch' to windows
sendKeyWrite :: CanSendKey m env => KeySwitch -> m ()
sendKeyWrite s = view ptr >>= \p -> liftIO $
  poke p (s^.re _KeySwitch) >> c_sendKey p

-- | Run some function in the context of an acquired SendKey device
withSendKey :: MonadUnliftIO m => (SendKeyEnv -> m a) -> m a
withSendKey = bracket (liftIO openSendKey) (liftIO . closeSendKey)
