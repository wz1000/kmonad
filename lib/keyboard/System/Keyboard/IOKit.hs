{-# LANGUAGE CPP #-}
-- |

module System.Keyboard.IOKit where

import Prelude
import System.Keyboard.Types

import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

-- | The runtime environment for IOKit computations.
newtype IOKitEnv = IOKitEnv

-------------------------------------------------------------------------------
-- $ffi

#if defined darwin_HOST_OS

-- | Use the mac c-api to grab keyboard(s)
foreign import ccall "grab_kb"
  grab_kb :: CString -> IO Word8

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: IO Word8

-- | Pass a pointer to a buffer to wait_key, and when it
-- returns, the buffer can be read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr MacPacket -> IO Word8

#else

grab_kb :: CString -> IO Word8
grab_kb _ = throwIO $ FFIWrongOS "acquire an IOKit keyboard" Mac

release_kb :: IO Word8
release_kb = throwIO $ FFIWrongOS "release an IOKit keyboard" Mac

wait_key :: Ptr MacPacket -> IO Word8
wait_key = throwIO $ FFIWrongOS "wait for an IOKit event" Mac

#endif

grabKb :: CString -> IO ()
grabKb = void . grab_kb

releaseKb :: IO ()
releaseKb = void release_kb


waitKey :: Ptr MacPacket -> IO MacPacket
waitKey ptr = do
       re <- wait_key ptr >> peek ptr
       -- Filter unwanted keycodes sent on each key event.
       case (re^.reCode) of
         (0x7, 0xFFFFFFFF) -> waitKey ptr
         (0x7, 0x1)        -> waitKey ptr
         _                 -> return re

-------------------------------------------------------------------------------


withIOKitSource :: MonadUnliftIO m => IOKitCfg -> (Ctx r m GetKey
withIOKitSource cfg = mkCtx $ \f -> do

  let prodStr :: Maybe Text
      prodStr = cfg^.productStr

  let init :: LUIO m e => m (Ptr MacPacket)
      init = do
        case prodStr of
          Nothing -> do
            logInfo "Opening HID devices"
            liftIO $ grabKb nullPtr
          Just s -> do
            logInfo $ "Opening HID device: " <> s
            liftIO $ withCString (unpack s) grabKb

        liftIO . mallocBytes $ sizeOf (undefined :: MacPacket)

  let cleanup :: LUIO m e => Ptr MacPacket -> m ()
      cleanup ptr = do
        case prodStr of
          Nothing -> logInfo "Closing HID devices"
          Just s -> logInfo $ "Closing HID device: " <> s

        liftIO $ releaseKb >> free ptr

  let nextEvent :: IO m => Ptr MacPacket -> m KeySwitch
      nextEvent ptr = do
        re <- liftIO $ waitKey ptr
        pure $ mkKeySwitch (if re^.reVal == 0 then Release else Press)
                           (Keycode $ re^.reCode)

  bracket init cleanup $ \ptr -> f (nextEvent ptr)
