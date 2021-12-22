{-# OPTIONS_GHC -Wno-orphans #-} -- <- Because CanWithInput
{-# LANGUAGE CPP #-}

-- | TODO: Document me

module Keyboard.I.Evdev
  ( -- * Cfg
    EvdevCfg
  , HasEvdevCfg

    -- * Exceptions
  , EvdevException(..)
  , AsEvdevException(..)

    -- * API
  , evdevRead
  , withEvdev
  )
where

import Preface

import Keyboard.Types.Linux

import System.Posix
import RIO.FilePath ((</>))
import RIO.List (find, isSuffixOf)
import UnliftIO.Directory (listDirectory)

import qualified RIO.ByteString as B
import qualified Data.Serialize as B (decode)


{- NOTE: types -----------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | The configuration record for evdev key-input on Linux
--
-- If no path is passed to Evdev, we try to autodetect the keyboard by matching
-- the first keyboard under @/dev/input/by-path@ that ends in @kbd@
newtype EvdevCfg = EvdevCfg
  { _evdevPath :: Maybe FilePath -- ^ The path to the device-file
  } deriving (Eq, Show)
makeClassy ''EvdevCfg

-- | The environment used to handle evdev operations
data EvdevEnv = EvdevEnv
  { _cfg :: EvdevCfg -- ^ The 'EvdevCfg' with which this env was started
  , _pth :: FilePath -- ^ Path to the device file we opened
  , _fd  :: CInt     -- ^ Open file-descriptor to the Evdev keyboard
  , _h   :: Handle   -- ^ Haskell file handle to the same file
  } deriving (Eq, Show)
makeClassy ''EvdevEnv

-- NOTE: `fd` and `h` point to the same file. I thought to refactor this and
-- only hold 1 reference, and only reconsruct `h` when I needed to from `fd`.
-- THIS BREAKS EVERYTHING AND SHOULD NOT BE DONE.

instance HasEvdevCfg EvdevEnv where evdevCfg = cfg

{- NOTE: Exceptions ------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | All the things that can go wrong
data EvdevException
  = EvdevCouldNotDetect
  -- ^ Could not autodetect a fitting keyboard
  | EvdevCouldNotAcquire FilePath ReturnCode
  -- ^ Could not acquire IOCTL grab
  | EvdevCouldNotRelease EvdevEnv ReturnCode
  -- ^ Could not release IOCTL grab
  | EvdevCouldNotRead    EvdevEnv
  -- ^ Could not read from evdev file
  | EvdevCouldNotDecode  EvdevEnv String
  -- ^ Received unparseable input
  deriving Show
makeClassyPrisms ''EvdevException

-- | How to display EvdevExceptions
instance Exception EvdevException where
  displayException EvdevCouldNotDetect = concat
    [ "Could not autodetect valid keyboard, please specify one in "
    , "config or invocation."]
  displayException (EvdevCouldNotAcquire p n) = concat
    [ "Failed to acquire ioctl-grab on: '", p
    , "' with errorcode: ", show n ]
  displayException (EvdevCouldNotRelease c n) = concat
    [ "Failed to release ioctl-grab on: '", c^.pth
    , "' with errorcode: ", show n ]
  displayException (EvdevCouldNotRead c) = concat
    [ "Could not read from: '", c^.pth]
  displayException (EvdevCouldNotDecode c t) = concat
    [ "Failed to parse event from: '", c^.pth
    , "' with error message: ", t ]

instance AsEvdevException SomeException where _EvdevException = exception

{- NOTE: FFI -------------------------------------------------------------------
-------------------------------------------------------------------------------}

#if defined linux_HOST_OS

foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard ::
       CInt        -- ^ The open file-handle to ioctl
    -> CInt        -- ^ 1 to grab, 0 to release
    -> OnlyIO CInt -- ^ IO action with return-code

#else

c_ioctl_keyboard :: CInt -> CInt -> OnlyIO CInt
c_iotctl_keyboard _ _ =
  throwIO $ FFIWrongOS "acquire a linux IOCTL grab" "linux"

#endif

{- NOTE: internal --------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | Open a device file and execute an ioctl grab
openEvdev :: EvdevCfg -> OnlyIO EvdevEnv
openEvdev cfg = do
  pth <- case cfg^.evdevPath of
    Just p -> pure p
    Nothing -> do
      ps <- listDirectory "/dev/input/by-path"
      case find ("kbd" `isSuffixOf`) ps of
        Just p  -> pure $ "/dev/input/by-path" </> p
        Nothing -> throwing _EvdevCouldNotDetect ()

  let flags = OpenFileFlags False False False False False
  (Fd fd_) <- openFd pth ReadOnly Nothing flags
  h        <- fdToHandle (Fd fd_)
  c_ioctl_keyboard fd_ 1 `onNonZeroThrow` (_EvdevCouldNotAcquire, pth)
  pure $ EvdevEnv cfg pth fd_ h

-- | Execute an ioctl release and close a device file
closeEvdev :: EvdevEnv -> OnlyIO ()
closeEvdev env = c_ioctl_keyboard (env^.fd) 0
                   `onNonZeroThrow` (_EvdevCouldNotRelease, env)
                   `finally` (closeFd . Fd $ env^.fd)

{- NOTE: API -------------------------------------------------------------------
-------------------------------------------------------------------------------}

-- | Read a linux-event from the device
evdevRead :: RIO EvdevEnv LowLinEvent
evdevRead = view evdevEnv >>= \env -> liftIO $ do

  bytes <- B.hGet (env^.h) 24
  case B.decode . B.reverse $ bytes of
    Left s            -> throwing _EvdevCouldNotDecode (env, s)
    Right (a,b,c,d,e) -> pure $ (LinPacket e d c b a)^.re _LinPacket

-- | Run some function in the context of an acquired evdev keyboard.
withEvdev :: MonadUnliftIO m => EvdevCfg -> (OnlyIO LowLinEvent -> m a) -> m a
withEvdev cfg f = bracket (liftIO $ openEvdev cfg) (liftIO . closeEvdev)
  $ \env -> f (runRIO env evdevRead)
