{-# LANGUAGE CPP #-}
-- | TODO: Document me
module System.Keyboard.IO.Uinput
  ( -- * Basic types
    UinputCfg(..)
  , HasUinputCfg(..)
  , UinputEnv
  , CanUinput

    -- * Exceptions
  , UinputException(..)
  , AsUinputException(..)

    -- * API
  , uinputWriteLow
  , uinputWrite
  , uinputRepeat
  , withUinput
  )
where

import Prelude

import System.Keyboard.Types
import Util.FFI

import System.IO.Error.Lens

import Foreign.C.String
import Foreign.C.Types
import System.Posix


{- NOTE: Types -----------------------------------------------------------------
The configuration and environment settings for a Uinput sink.
-------------------------------------------------------------------------------}

-- | Configuration of the Uinput keyboard to instantiate
data UinputCfg = UinputCfg
  { _vendorCode     :: !Int  -- ^ USB vendor code of the generated keyboard
  , _productCode    :: !Int  -- ^ USB product code of the generated keyboard
  , _productVersion :: !Int  -- ^ USB product version
  , _keyboardName   :: !Name -- ^ Name used to identify keyboard to OS
  } deriving (Eq, Show)
makeClassy ''UinputCfg

instance Default UinputCfg where
  def = UinputCfg
    { _vendorCode     = 0xFFFF
    , _productCode    = 0xFFFF
    , _productVersion = 0x0000
    , _keyboardName   = "Haskell simulated keyboard" }

-- | The environment used to handle uinput operations
data UinputEnv = UinputEnv
  { _cfg :: UinputCfg -- ^ The configuration with which we were started
  , _fd  :: CInt      -- ^ Open file-descriptor to the uinput keyboard
  }
makeClassy ''UinputEnv

instance HasUinputCfg UinputEnv where uinputCfg = cfg

-- | A constraint synonym for the required context to run the low-level API
--
-- To run a Uinput computation we need IO and access to a 'UinputEnv' value.
type CanUinput m env = (MonadIO m, MonadReader env m, HasUinputEnv env)

{- NOTE: Exceptions ------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | Exceptions that can be raised when creating or destroying Uinput devices
data UinputException
  = UinputPermissionDenied
  -- ^ Did not have permission to open file
  | UinputCouldNotCreate  UinputCfg ReturnCode
  -- ^ Could not create uinput device
  | UinputCouldNotDestroy UinputCfg ReturnCode
  -- ^ Could not destroy uinput device
  | UinputCouldNotWrite UinputCfg ReturnCode
  -- ^ Could not write to uinput device
  deriving Show
makeClassyPrisms ''UinputException

-- Pretty-printing descriptions for the UinputExceptions
instance Exception UinputException where
  displayException UinputPermissionDenied =
    "Permission error: did you remember to call `modprobe uinput`?"
  displayException (UinputCouldNotCreate _ i) =
    "Failed to create uinput device, got error code: " <> show i
  displayException (UinputCouldNotDestroy _ i) =
    "Failed to cleanup uinput device, got error code: " <> show i
  displayException (UinputCouldNotWrite _ i) =
    "Failed to write to uinput device, got error code: " <> show i

instance AsUinputException SomeException where _UinputException = exception

{- NOTE: FFI -------------------------------------------------------------------
We use 3 different FFI calls. Note that the way the CPP directives have been
structured means that this code *will compile* on any OS. This is important for
ease of debugging and sharing. However, during runtime, on nonLinux, this will
throw an informative error about how uinput-sinks only work on Linux.
-------------------------------------------------------------------------------}
#if defined linux_HOST_OS

foreign import ccall "acquire_uinput_keysink"
  c_acquire_uinput_keysink
    :: CInt    -- ^ Posix handle to the file to open
    -> CString -- ^ Name to give to the keyboard
    -> CInt    -- ^ Vendor ID
    -> CInt    -- ^ Product ID
    -> CInt    -- ^ Version ID
    -> IO CInt

foreign import ccall "release_uinput_keysink"
  c_release_uinput_keysink :: CInt -> IO CInt

foreign import ccall "send_event"
  c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

#else

c_acquire_uinput_keysink :: CInt -> CString -> CInt -> CInt -> CInt -> IO CInt
c_acquire_uinput_keysink _ _ _ _ _ =
  throwIO $ FFIWrongOS "open a uinput sink" "linux"

c_release_uinput_keysink :: CInt -> IO Int
c_release_uinput_keysink _ =
  throwIO $ FFIWrongOS "release a uinput sink" "linux"

c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
c_send_event _ _ _ _ _ _ =
  throwIO $ FFIWrongOS "send an event to a uinput sink" "linux"

#endif

{- NOTE: internal --------------------------------------------------------------
The following code is internal to the module. We simply use IO and don't bother
with flexible monad constraints. These are essentially smart wrappers around the
FFI-calls above.
-------------------------------------------------------------------------------}

-- | Create and register our uinput device
openUinput :: UinputCfg -> IO UinputEnv
openUinput cfg = do
  -- Open the file-handle to the standard uinput device
  let flags = OpenFileFlags False False False True False
  (Fd fd_) <- catching _IOException (openFd "/dev/uinput" WriteOnly Nothing flags)
    $ \err -> if is _PermissionDenied (err^.errorType)
              then throwIO UinputPermissionDenied
              else throwIO err

  -- Register our file descriptor with the uinput kernel module
  cstr <- newCString . unpack $ cfg^.keyboardName
  c_acquire_uinput_keysink fd_ cstr
    (fi $ cfg^.vendorCode) (fi $ cfg^.productCode) (fi $ cfg^.productVersion)
      `onNonZeroThrow` (_UinputCouldNotCreate, cfg)
  pure $ UinputEnv cfg fd_

-- | Unregister and close our uinput device
closeUinput :: UinputEnv -> IO ()
closeUinput env = c_release_uinput_keysink (env^.fd)
                    `onNonZeroThrow` (_UinputCouldNotDestroy, env^.cfg)
                    `finally`        (closeFd (Fd $ env^.fd))

{- NOTE: API -------------------------------------------------------------------
The following code gets exposed to the users of this library. We provide
polymorphic monad constraints to allow for easier use of these functions, and we
make sure not to expose any of the internal technicalities.
-------------------------------------------------------------------------------}

-- | Write a 'LowLinEvent' to the uinput sink
--
-- This can write any 'LowLinEvent' to the OS, allowing for manual handling of
-- sync and repeat events. For a simpler but less flexible method, see
-- 'uinputWrite' below.
uinputWriteLow :: CanUinput m env => LowLinEvent -> m ()
uinputWriteLow e = view uinputEnv >>= \env -> liftIO $
  c_send_event (env^.fd)
    (e^.linType._fi) (e^.linCode._fi) (e^.linVal._fi) (e^.linS._fi) (e^.linNS._fi)
      `onNonZeroThrow` (_UinputCouldNotWrite, env^.cfg)

-- | Send a 'KeySwitch' to the uinput sink. Automatically syncs.
uinputWrite :: CanUinput m env => KeySwitch -> m ()
uinputWrite e = do
  uinputWriteLow . LowLinKeyEvent  $ e^.re _KeySwitch
  uinputWriteLow . LowLinSyncEvent $ mkLinSyncEvent

-- | Tell uinput to do a key-repeat. Automatically syncs.
uinputRepeat :: CanUinput m env => Keycode -> m ()
uinputRepeat c = do
  uinputWriteLow . LowLinRepeatEvent $ mkLinRepeatEvent (c^.re _Keycode)
  uinputWriteLow . LowLinSyncEvent   $ mkLinSyncEvent

-- | Run some function in the context of an open uinput device.
withUinput :: MonadUnliftIO m => UinputCfg -> (UinputEnv -> m a) -> m a
withUinput cfg = bracket (liftIO $ openUinput cfg) (liftIO . closeUinput)
