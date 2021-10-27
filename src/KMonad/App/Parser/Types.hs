{-|
Module      : KMonad.App.Parser.Types
Description : The basic types of configuration parsing.
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.App.Parser.Types
  -- ( -- * $bsc
--     Parser
--   , PErrors(..)

--     -- * $cfg
--   , CfgToken(..)

--     -- * $but
--   , DefButton(..)

--     -- * $tls
--   , DefSetting(..)
--   , DefSettings
--   , DefAlias
--   , DefLayer(..)
--   , DefSrc
--   , KExpr(..)

--     -- * $defio
--   -- , IToken(..)
--   -- , OToken(..)

--     -- * $lenses
--   , AsKExpr(..)
--   , AsDefSetting(..)

--     -- * $reexport
--   , module X
-- )
where


import KMonad.Prelude
-- import KMonad.App.KeyIO
import KMonad.App.Types
import KMonad.Model.Types
import KMonad.Pullchain.Button
import KMonad.Pullchain.Types
-- import KMonad.Util.Keyboard
-- import KMonad.Keyboard.IO
import KMonad.Util hiding (Keycode)

import KMonad.App.Configurable

import System.Keyboard hiding (Name)

import Text.Megaparsec      hiding (parse)
import Text.Megaparsec.Char

{- SECTION: Types -------------------------------------------------------------}

{- SUBSECTION: Env ------------------------------------------------------------}

-- | The environment we need to run a parser
data ParseEnv = ParseEnv
  { _pKeyTable   :: KeyTable
  , _composeCode :: Keycode
  , _shiftCode   :: Keycode
  } deriving (Eq, Show)
makeClassy ''ParseEnv

instance HasKeyTable ParseEnv where keyTable = pKeyTable

{- SUBSECTION: Errors ---------------------------------------------------------}

data MyError
  = UnknownComposeKey Keyname -- ^ Failed to look up the compose key
  | UnknownShiftKey   Keyname -- ^ Failed to look up the standard shift key
  | NoKeycodeFor Keyname      -- ^ Failed to lookup keycode
  deriving (Eq, Ord, Show)

-- FIXME: Might want some better string representations here
instance Exception MyError where
  displayException (NoKeycodeFor n) = "Porque? " <> unpack n
  displayException (UnknownComposeKey n) = "Pourquoi? " <> unpack n
  displayException (UnknownShiftKey n) = "Waarom? " <> unpack n

instance ShowErrorComponent MyError where
  showErrorComponent = displayException

{- SUBSECTION: 'narrowed' Megaparsec types ------------------------------------}

-- | Parser's operate on Text and carry no state
-- type Parser = Parsec Void Text
type P a = ParsecT MyError Text (Reader ParseEnv) a

-- | The type of errors returned by the Megaparsec parsers
newtype PErrors = PErrors (ParseErrorBundle Text MyError)
  deriving Eq

instance Show PErrors where
  show (PErrors e) = "Parse error at " <> errorBundlePretty e

instance Exception PErrors

--------------------------------------------------------------------------------
-- $but
--
-- Tokens representing different types of buttons


-- NOTE: Not sure if this should live here.. App.Types is better I think
data ModKey
  = Ctrl  | Shift  | Alt  | Meta | RCtrl | RShift | RAlt | RMeta
  deriving (Eq, Show)

-- | Button ADT
data DefButton
  = KRef Text                              -- ^ Reference a named button
  | KSimple Keyname                        -- ^ Emit a keycode by its name, either normal or shifted
  | KModded ModKey DefButton               -- ^ Modded version of some button
  | KLayerToggle Text                      -- ^ Toggle to a layer when held
  | KLayerSwitch Text                      -- ^ Switch base-layer when pressed
  | KLayerAdd Text                         -- ^ Add a layer when pressed
  | KLayerRem Text                         -- ^ Remove top instance of a layer when pressed
  | KTapNext DefButton DefButton           -- ^ Do 2 things based on behavior
  | KTapHold Int DefButton DefButton       -- ^ Do 2 things based on behavior and delay
  | KTapHoldNext Int DefButton DefButton   -- ^ Mixture between KTapNext and KTapHold
  | KTapNextRelease DefButton DefButton    -- ^ Do 2 things based on behavior
  | KTapHoldNextRelease Int DefButton DefButton
    -- ^ Like KTapNextRelease but with a timeout
  | KAroundNext DefButton                  -- ^ Surround a future button
  | KAroundNextSingle DefButton            -- ^ Surround a future button
  | KMultiTap [(Int, DefButton)] DefButton -- ^ Do things depending on tap-count
  | KAround DefButton DefButton            -- ^ Wrap 1 button around another
  | KAroundNextTimeout Int DefButton DefButton
  | KTapMacro [DefButton] (Maybe Int)
    -- ^ Sequence of buttons to tap, possible delay between each press
  | KTapMacroRelease [DefButton] (Maybe Int)
    -- ^ Sequence of buttons to tap, tap last on release, possible delay between each press
  | KComposeSeq [DefButton]                -- ^ Compose-key sequence
  | KPause Ms                    -- ^ Pause for a period of time
  | KLayerDelay Int Name               -- ^ Switch to a layer for a period of time
  | KLayerNext Name                    -- ^ Perform next button in different layer
  | KCommand Text (Maybe Text)             -- ^ Execute a shell command on press, as well
                                           --   as possibly on release
  | KStickyKey Int DefButton               -- ^ Act as if a button is pressed for a period of time
  | KTrans                                 -- ^ Transparent button that does nothing
  | KBlock                                 -- ^ Button that catches event
  deriving Show


--------------------------------------------------------------------------------
-- $cfg
--
-- The Cfg token that can be extracted from a config-text without ever enterring
-- IO. This will then directly be translated to a DaemonCfg
--

-- | The 'CfgToken' contains all the data needed to construct an
-- 'KMonad.App.AppCfg'.
data CfgToken = CfgToken
  { _src   :: InputCfg  -- ^ How to grab the source keyboard
  , _snk   :: OutputCfg -- ^ How to construct the out keybboard
  , _km    :: Keymap BCfg  -- ^ A collection of layers of button configurations
  , _fstL  :: Name         -- ^ Name of initial layer
  , _flt   :: Bool         -- ^ How to deal with unhandled events
  , _allow :: Bool         -- ^ Whether to allow shell commands
  } deriving Show
makeClassy ''CfgToken


--------------------------------------------------------------------------------
-- $tls
--
-- A collection of all the different top-level statements possible in a config
-- file.

-- | A list of keycodes describing the ordering of all the other layers
type DefSrc = [Keycode]

-- | A mapping from names to button tokens
type DefAlias = [(Text, DefButton)]

-- | A layer of buttons
data DefLayer = DefLayer
  { _layerName :: Text        -- ^ A unique name used to refer to this layer
  , _buttons   :: [DefButton] -- ^ A list of button tokens
  }
  deriving Show


--------------------------------------------------------------------------------
-- $defcfg
--
-- Different settings



-- | All possible single settings
data DefSetting
  = SIToken      InputToken
  | SOToken      OutputToken
  | SCmpSeq      DefButton
  | SInitStr     Text
  | SFallThrough Bool
  | SAllowCmd    Bool
  | SCmpSeqDelay Int
  deriving Show
makeClassyPrisms ''DefSetting

-- | 'Eq' instance for a 'DefSetting'. Because every one of these options may be
-- given at most once, we only need to check the outermost constructor in order
-- to test for equality
instance Eq DefSetting where
  SIToken{}      == SIToken{}      = True
  SOToken{}      == SOToken{}      = True
  SCmpSeq{}      == SCmpSeq{}      = True
  SInitStr{}     == SInitStr{}     = True
  SFallThrough{} == SFallThrough{} = True
  SAllowCmd{}    == SAllowCmd{}    = True
  _              == _              = False

-- | A list of different 'DefSetting' values
type DefSettings = [DefSetting]

--------------------------------------------------------------------------------
-- $tkn

-- | Any statement in a config-file must parse to a 'KExpr'
data KExpr
  = KDefCfg   DefSettings
  | KDefSrc   DefSrc
  | KDefLayer DefLayer
  | KDefAlias DefAlias
  deriving Show
makeClassyPrisms ''KExpr


--------------------------------------------------------------------------------
-- $act
