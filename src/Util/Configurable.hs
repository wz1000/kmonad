{-# LANGUAGE ExistentialQuantification #-}
module Util.Configurable where

import Preface
import Util.Time

import Data.Monoid
import Options.Applicative (ReadM, eitherReader)


{- SECTION: Basic types -------------------------------------------------------}

type FlagName   = Name -- ^ A name that refers to a flag
type OptionName = Name -- ^ A name that refers to an option

{- SECTION: Errors ------------------------------------------------------------}

-- | Things that can go wrong with configurables
data ConfigurableException
  = UnknownConfigurable Name
  -- ^ Encountered a refence to an unknown configurable
  | BadConfigurableValue Name Text Name
  -- ^ Encountered a bad value for a known configurable
  deriving (Eq, Show)

instance Exception ConfigurableException where
  displayException (UnknownConfigurable n) = unpack . mconcat $
    ["Encountered unknown setting: " <> n]
  displayException (BadConfigurableValue n t m) = unpack . mconcat $
    ["Encountered unreadable value for '", n, "': ", t
    , ". Expecting a ", m, " value." ]

{- SECTION : Change -----------------------------------------------------------}

-- | A datatype representing some change to a structure
data Change s = Change
  { _cDescrip :: Description
  , _endo     :: Endo s
  }
makeClassy ''Change

instance HasDescription (Change s) where description = cDescrip

-- | A list of changes, we make heavy use of the monoid properties
type Changes s = [Change s]

-- | Create a new 'Changes' value
mkChange :: Text -> (s -> s) -> Changes s
mkChange t = singleton . Change t . Endo

-- | Create a 'Change' that sets some property to some value
setProp :: Text -> Traversal' s a -> a -> Changes s
setProp t l a = mkChange t (\s -> s & l .~ a)

-- | Lift an endo into an endo over some larger structure using a traversal
liftEndo :: Traversal' s a -> Endo a -> Endo s
liftEndo l (Endo f) = (Endo $ over l f)

-- | Lift a change into some larger structure with a traversal describing the embedding
liftChange :: Traversal' s a -> Change a -> Change s
liftChange l (Change d e) = Change d (liftEndo l e)

{- SECTION: ArgParse -----------------------------------------------------------
I'd like to use ReadM from optparse-applicative directly, but I can't find a way
to just run a ReadM on some text. So instead we just write a very small, simple
reader that can be exported to ReadM for the optparse stuff, and can be used in
our config parser directly. That way we don't write the parsers twice.
-------------------------------------------------------------------------------}

-- | A wrapper around a ReaderT that turns text into maybe value
newtype ArgParse a = ArgParse { _uArgParse :: ReaderT Text Maybe a }
  deriving (Functor, Applicative, Monad, MonadReader Text)
makeClassy ''ArgParse

-- | Run a 'ArgParse' on some text and return the arg or fail
runArgParse :: HasArgParse s a => s -> Text -> Maybe a
runArgParse p = runReaderT (p^.argParse.uArgParse)

-- | An ArgParse that uses a types 'Read' instance
readParse :: Read a => ArgParse a
readParse = ArgParse . ReaderT $ readMaybe . unpack

-- | An ArgParse that wraps a simple parsing function
parseWith :: (Text -> Maybe a) -> ArgParse a
parseWith = ArgParse . ReaderT

-- | An ArgParse that matches names to values
parseMatch :: [(Text, a)] -> ArgParse a
parseMatch = parseWith . flip lookup

-- | An 'ArgType' value, pairing some parser with a name
data Arg a = Arg
  { _argName  :: Name
  , _aArgParse :: ArgParse a
  }
makeClassy ''Arg

instance HasName     (Arg a)   where name     = argName
instance HasArgParse (Arg a) a where argParse = aArgParse

instance Functor Arg where
  fmap f a = Arg (a^.argName) (fmap f $ a^.argParse)

{- SUBSECTION: Flag -----------------------------------------------------------}

-- | A datatype representing some fixed change to a structure
data Flag s = Flag
  { _fName        :: FlagName
  , _fDescription :: Text
  , _fChange      :: Changes s}
makeLenses ''Flag

instance HasName (Flag s) where name = fName
instance HasDescription (Flag s) where description = fDescription

-- | Create a flag that sets some traversal to some fixed value when triggered
mkFlag :: FlagName -> Traversal' s a -> a -> Text -> Flag s
mkFlag n l a d = Flag n d $ setProp ("flag:" <> n) l a

-- | Extract the change from a flag
runFlag :: Flag s -> Changes s
runFlag = _fChange

-- | Existential wrapper around a flag specified on some existing cfg and an
-- embedding of that cfg into a RootCfg
data AnyFlag cfg = forall s. AnyFlag
  { fEmbed :: Traversal' cfg s
  , fFlag  :: Flag s
  }
-- NOTE: this doesn't work here: makeLenses ''AnyFlag. Because existential?

{- NOTE: These have to be done manually too, I can never 'get at' the flag in an
 AnyFlag, because that would leak a wrapped type. However, I can always get a
 name and description. -}
instance HasName (AnyFlag cfg) where
  name = lens
    (\(AnyFlag _ f)   -> f^.name)
    (\(AnyFlag e f) n -> AnyFlag e $ f & name .~ n)

instance HasDescription (AnyFlag cfg) where
  description = to (\(AnyFlag _ f)   -> f^.description)

-- | How to apply 'AnyFlag' to a 'RootCfg'
runAnyFlag :: AnyFlag f -> Changes f
runAnyFlag (AnyFlag l f) = map (liftChange l) $ runFlag f

{- SUBSECTION: Option ---------------------------------------------------------}

-- | A datatype representing some settable option on a structure
data Option s = forall a. Option
  { _oName        :: OptionName
  , _oDescription :: Text
  , _oArg         :: Arg a
  , _oChange      :: a -> Changes s }
makeLenses ''Option

instance HasName        (Option s) where name        = oName
instance HasDescription (Option s) where description = oDescription

-- | Create an option that sets some traversal to some passed value when called
mkOption :: Show a
  => OptionName -> Traversal' s a -> Arg a -> Text -> Option s
mkOption n l a d = Option n d a
  (\x -> mkChange ("option:" <> n <> ":" <> tshow x) $ set l x)

-- | Existential wrapper around an option on some configuration and an embedding
-- of that configuration into a RootCfg
data AnyOption cfg = forall s. AnyOption
  { oEmbed  :: Traversal' cfg s
  , oOption :: Option s}

instance HasName (AnyOption cfg) where
  name = lens
    (\(AnyOption _ o)   -> o^.name)
    (\(AnyOption e o) n -> AnyOption e $ o & name .~ n)

instance HasDescription (AnyOption cfg) where
  description = to $ \(AnyOption _ o) -> o^.description

-- | Apply an option-change to a RootCfg
runAnyOption :: AnyOption cfg -> Text -> Either ConfigurableException (Changes cfg)
runAnyOption (AnyOption l o) t = map (liftChange l) <$> readOption o t

{- SUBSECTION: Collections ----------------------------------------------------}

collectFlags :: Traversal' s a -> [Flag a] -> Named (AnyFlag s)
collectFlags l = byName . map (AnyFlag l)

collectOpts :: Traversal' s a -> [Option a] -> Named (AnyOption s)
collectOpts l = byName . map (AnyOption l)


{- SECTION: Operations --------------------------------------------------------}


{- SUBSECTION: ArgParse -------------------------------------------------------}

-- | View anything that has an 'Arg' as an optparse-applicate 'ReadM'
readM :: HasArg s a => Getter s (ReadM a)
readM = to $ eitherReader . f
  where
    f a s = case runArgParse (a^.arg.argParse) . pack $ s of
      Nothing -> Left . unpack $ "Could not parse: " <> a^.argName
      Just a  -> Right a

{- SUBSECTION: Change operations ----------------------------------------------}

-- | Try to apply an option by parsing its arg from some text
readOption :: Option s -> Text -> Either ConfigurableException (Changes s)
readOption (Option n _ a c) t = maybe e (Right . c) . runArgParse a $ t
  where e = Left $ BadConfigurableValue n t (a^.argName)

-- | Apply a change to some structure
appChange :: Changes s -> s -> s
appChange = appEndo . mconcat . toListOf (folded.endo)

-- | Apply a change to some default value
onDef :: Default s => Changes s -> s
onDef = (`appChange` def)

{- SECTION: Values ------------------------------------------------------------}

{- SUBSECTION: ArgParse -------------------------------------------------------}

-- | An argument that accepts pure text
txtArg :: Arg Text
txtArg = Arg "raw text" $ ask

-- | An argument that accepts pure text as a filepath
--
-- TODO: Could add validation here
fileArg :: Arg FilePath
fileArg = unpack <$> txtArg

-- | An argument that accepts integers
intArg :: Arg Int
intArg = Arg "integer" $ readParse

-- | An argument that accepts integers as milliseconds
msArg :: Arg Ms
msArg = fi <$> intArg

-- | Parse 'on' as True and 'off' as False
onOffArg :: Arg Bool
onOffArg = Arg "on or off" . parseMatch $
  [("on", True) , ("off", False)]
