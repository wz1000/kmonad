{-# LANGUAGE CPP #-}
-- | TODO: Document me
module System.Keyboard.IO.Evdev
  ( -- * Basic types
    EvdevCfg(..)
  , HasEvdevCfg(..)
  , EvdevEnv
  , CanEvdev

    -- * Exceptions
  , EvdevException(..)
  , AsEvdevException(..)

    -- * API
  , evdevReadLow
  , evdevRead
  , withEvdev
  )
where

import Prelude

import System.Keyboard.Types
import Util.FFI

import Foreign.C.String
import Foreign.C.Types
import System.Posix

import qualified RIO.ByteString as B
import qualified Data.Serialize as B (decode)


{- NOTE: types -----------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | The configuration record for evdev key-input on Linux
newtype EvdevCfg = EvdevCfg
  { _evdevPath :: FilePath -- ^ The path to the input-file to open and capture
  } deriving Show
makeClassy ''EvdevCfg

-- | The environment used to handle evdev operations
data EvdevEnv = EvdevEnv
  { _cfg :: EvdevCfg -- ^ The 'EvdevCfg' with which this env was started
  , _fd  :: CInt     -- ^ Open file-descriptor to the Evdev keyboard
  , _h   :: Handle   -- ^ Haskell file handle to the same file
  }
makeClassy ''EvdevEnv

-- NOTE: `fd` and `h` point to the same file. I thought to refactor this and
-- only hold 1 reference, and only reconsruct `h` when I needed to from `fd`.
-- THIS BREAKS EVERYTHING AND SHOULD NOT BE DONE.

instance HasEvdevCfg EvdevEnv where evdevCfg = cfg

type CanEvdev m env = (MonadIO m, MonadReader env m, HasEvdevEnv env)

{- NOTE: Exceptions ------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | All the things that can go wrong
data EvdevException
  = EvdevCouldNotAcquire EvdevCfg ReturnCode
  -- ^ Could not acquire IOCTL grab
  | EvdevCouldNotRelease EvdevCfg ReturnCode
  -- ^ Could not release IOCTL grab
  | EvdevCouldNotRead    EvdevCfg
  -- ^ Could not read from evdev file
  | EvdevCouldNotDecode  EvdevCfg String
  -- ^ Received unparseable input
  deriving Show
makeClassyPrisms ''EvdevException

-- | How to display EvdevExceptions
instance Exception EvdevException where
  displayException (EvdevCouldNotAcquire c n) = concat
    [ "Failed to acquire ioctl-grab on: '", c^.evdevPath
    , "' with errorcode: ", show n ]
  displayException (EvdevCouldNotRelease c n) = concat
    [ "Failed to release ioctl-grab on: '", c^.evdevPath
    , "' with errorcode: ", show n ]
  displayException (EvdevCouldNotRead c) = concat
    [ "Could not read from: '", c^.evdevPath]
  displayException (EvdevCouldNotDecode c t) = concat
    [ "Failed to parse event from: '", c^.evdevPath
    , "' with error message: ", t ]

instance AsEvdevException SomeException where _EvdevException = exception

{- NOTE: FFI -------------------------------------------------------------------
-------------------------------------------------------------------------------}

#if defined linux_HOST_OS

foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard ::
       CInt    -- ^ The open file-handle to ioctl
    -> CInt    -- ^ 1 to grab, 0 to release
    -> IO CInt -- ^ IO action with return-code

#else

c_ioctl_keyboard :: CInt -> CInt -> IO CInt
c_iotctl_keyboard _ _ =
  throwIO $ FFIWrongOS "acquire a linux IOCTL grab" "linux"

#endif

{- NOTE: internal --------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | Open a device file and execute an ioctl grab
openEvdev :: EvdevCfg -> IO EvdevEnv
openEvdev cfg = do
  let flags = OpenFileFlags False False False False False
  (Fd fd_) <- openFd (cfg^.evdevPath) ReadOnly Nothing flags
  h        <- fdToHandle (Fd fd_)
  c_ioctl_keyboard fd_ 1 `onNonZeroThrow` (_EvdevCouldNotAcquire, cfg)
  pure $ EvdevEnv cfg fd_ h

-- | Execute an ioctl release and close a device file
closeEvdev :: EvdevEnv -> IO ()
closeEvdev env = c_ioctl_keyboard (env^.fd) 0
                   `onNonZeroThrow` (_EvdevCouldNotRelease, env^.cfg)
                   `finally` (closeFd . Fd $ env^.fd)

{- NOTE: API -------------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | Return the first 'LowLinEvent' from an open device file
evdevReadLow :: CanEvdev m env => m LowLinEvent
evdevReadLow = view evdevEnv >>= \env -> liftIO $ do

  bytes <- B.hGet (env^.h) 24
  case B.decode . B.reverse $ bytes of
    Left s            -> throwing _EvdevCouldNotDecode (env^.cfg, s)
    Right (a,b,c,d,e) -> pure $ (LinPacket e d c b a)^.re _LinPacket

-- | Return the first key press or release from an open device file
evdevRead :: CanEvdev m env => m KeySwitch
evdevRead = evdevReadLow >>= \case
  LowLinKeyEvent e -> pure $ mkKeySwitch (e^.switch) (e^.keycode)
  _                -> evdevRead

-- | Run some function in the context of an acquired evdev keyboard.
--
-- Use this if you want access to all of the events thrown by a linux keyboard
-- event file. Use 'withEvdev' if you want only key press and release events.
-- This context is a little bit less fleshed out than 'withEvdev', and you will
-- have to embed the environment and deal with the 'LowLinEvent's yourself.
withEvdevEnv :: MonadUnliftIO m => EvdevCfg -> (EvdevEnv -> m a) -> m a
withEvdevEnv cfg = bracket (liftIO $ openEvdev cfg) (liftIO . closeEvdev)

-- | Run some function in the context of an acquired device file
withEvdev :: MonadUnliftIO m => EvdevCfg -> (KeyI -> m a) -> m a
withEvdev cfg f = withEvdevEnv cfg $ \env -> f (KeyI (runRIO env evdevRead))


instance CanOpenKeyI EvdevCfg where withKeyI = withEvdev
