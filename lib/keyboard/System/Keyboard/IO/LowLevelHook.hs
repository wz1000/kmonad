{-# LANGUAGE CPP #-}
module System.Keyboard.IO.LLHook
where

import Prelude

import System.Keyboard.Types

import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable


{- NOTE: Types -----------------------------------------------------------------
-------------------------------------------------------------------------------}

data LLHookEnv = LLHookEnv
  { _thread :: !(Async Word8)   -- ^ The thread-id of the listen-process
  , _buffer :: !(Ptr WinPacket) -- ^ Buffer used to communicate with process
  }
makeClassy ''LLHookEnv

type CanLLHook m env = (MonadIO m, MonadReader env m, HasLLHookEnv env)

{- NOTE: FFI -------------------------------------------------------------------
-------------------------------------------------------------------------------}

#if defined mingw32_HOST_OS

-- | Register a keyboard hook with windows
foreign import ccall "grab_kb" grab_kb :: IO Word8

-- | Unregister a keyboard hook with windows
foreign import ccall "release_kb" release_kb :: IO Word8

-- | Pass a pointer for Windows to write to, block until for windows writes.
foreign import ccall "wait_key" wait_key :: Ptr WinPacket -> IO ()

#else

grab_kb :: IO Word8
grab_kb = throwIO $ FFIWrongOS "register a keyboard hook" "windows"

release_kb :: IO Word8
release_kb = throwIO $ FFIWrongOS "release a keyboard hook" "windows"

wait_key :: Ptr WinPacket -> IO ()
wait_key _ = throwIO $ FFIWrongOS "wait for a keyboard hook" "windows"

#endif

{- NOTE: internal --------------------------------------------------------------
-------------------------------------------------------------------------------}

openLLHook :: IO LLHookEnv
openLLHook = LLHookEnv
  <$> async grab_kb <*> mallocBytes (sizeOf (undefined :: WinPacket))

closeLLHook :: LLHookEnv -> IO ()
closeLLHook env = release_kb
  `finally` (cancel $ env^.thread) >> (free $ env^.buffer)


{- NOTE: API -------------------------------------------------------------------
-------------------------------------------------------------------------------}

readLLHook :: CanLLHook m env => m KeySwitch
readLLHook = view buffer >>= \b -> liftIO $
  wait_key b >> (view _KeySwitch <$> peek b)

withLLHook :: MonadUnliftIO m => (LLHookEnv -> m a) -> m a
withLLHook = bracket (liftIO openLLHook) (liftIO . closeLLHook)
