{-# LANGUAGE DeriveAnyClass #-}
-- |

module KMonad.App.Cmds where

import Preface
import KMonad.App.Logging
import KMonad.Util.Time

import UnliftIO.Process
import qualified RIO.HashMap as M

{- SECTION: types -------------------------------------------------------------}

{- SUBSECTION: cmd ------------------------------------------------------------}

-- | A command to execute
--
-- In addition to storing the actual command to execute we also keep track of
-- optional pauses before and after executing a command, and the option to fork
-- a command (instead of wait for it to block). Note that the `postDelay` does
-- nothing if the 'Cmd' forks.
--
-- The pausing is important, for example, when using @setxkbmap@ to change the
-- compose key in Linux after the uinput keyboard has been generated. The kernel
-- needs a bit of time to register this change, so a pause of 500ms or so is
-- prudent.
data Cmd = Cmd
  { _instr    :: String -- ^ The actual shell command to perform
  , _preWait  :: Ms     -- ^ How long to pause before execution
  , _postWait :: Ms     -- ^ How long to pause after execution
  , _forking  :: Bool   -- ^ Whether kmonad should block until cmd completes
  } deriving (Eq, Show)
makeClassy ''Cmd

instance Default Cmd where def = Cmd "" 0 0 False

-- | Create a simple command with default configuration settings
simpleCmd :: Text -> Cmd
simpleCmd t = def & instr .~ unpack t


{- SUBSECTION: cmd-hooks ------------------------------------------------------}

-- | When a command gets executed
data CmdHook
  = OnStart     -- ^ First thing after reading invoc and optionally cfg
  | PreAcquire  -- ^ Before acquiring any keyIO
  | PostAcquire -- ^ After acquiring keyIO
  | PreRelease  -- ^ Just before releasing keyIO
  | PostRelease -- ^ Just after releasing keyIO
  | OnExit      -- ^ Last thing
  deriving (Eq, Show, Generic, Hashable)

-- | The configuration of command-running
data CmdsCfg = CmdsCfg
  { _hooks   :: M.HashMap CmdHook Cmd -- ^ Cmds for specific hooks
  , _enabled :: Bool                  -- ^ If false, completely disable commands
  } deriving (Eq, Show)
makeClassy ''CmdsCfg

instance Default CmdsCfg where def = CmdsCfg M.empty False

-- | The runtime environment of command-running
data CmdsEnv = CmdsEnv
  { _ceCmdsCfg :: CmdsCfg -- ^ Copy of the config
  , _ceLogEnv  :: LogEnv  -- ^ Copy of the logging environment
  }
makeClassy ''CmdsEnv

instance HasCmdsCfg CmdsEnv where cmdsCfg = ceCmdsCfg
instance HasLogEnv  CmdsEnv where logEnv  = logEnv

type CanCmds m env = (EnvUIO m env, HasCmdsEnv env, HasLogEnv env)

-- | NOTE: Not sure how to tuck the log-env into the environment, because we are
-- instantiating logging and cmds at the same moment, this little hack is
-- required to log *inside* of the cfg. I guess that just means I shouldn't cram
-- both of these into 1 context, but that is a consideration for the future.
--
-- FIXME: fixme somehow
-- withCmds :: (CanLog m env, HasCmdsCfg c) => c -> (CmdsEnv -> m a) -> m a
-- withCmds c f = view logEnv >>= \logenv -> f (CmdsEnv (c^.cmdsCfg) logenv)
withCmds :: HasCmdsCfg c => LogEnv -> c -> (CmdsEnv -> m a) -> m a
withCmds l c f = f (CmdsEnv (c^.cmdsCfg) l)

{- SECTION: IO ----------------------------------------------------------------}

-- | Trigger the running of a 'Cmd'
--
-- If the 'Cmd' is set to forking, then the Async will correspond to the running
-- command. If not, the Async is empty and does nothing.
runCmd :: CanCmds m env => Cmd -> m ()
runCmd cmd = view (cmdsEnv.enabled) >>= \b -> if not b then nil else do
  let go = do
        sep >> log "Running cmd:" >> pp cmd
        wait $ cmd^.preWait
        void $ createProcess_ "kmonad:runCmd" (shell $ cmd^.instr)
          { close_fds = True }
          {- ^NOTE: close_fds is important because the inherited processes
              should not have access to our KeyIO files and such -}
        wait $ cmd^.postWait
  if (cmd^.forking)
    then void $ async go
    else go

-- | Trigger the execution of any 'Cmd's registered to a 'CmdHook'
triggerHook :: CanCmds m env => CmdHook -> m ()
triggerHook t = view (cmdsEnv.hooks.at t) >>= \case
  Nothing -> nil
  Just c  -> runCmd c
