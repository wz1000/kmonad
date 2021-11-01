module KMonad.Util.Logging.Operations
  (
    ppRecord
  )
where

import KMonad.Prelude
import qualified RIO.Text.Lazy as T (toStrict)
import Text.Pretty.Simple (pShow, pShowNoColor)


-- | Pretty-print any showable object, useful for configuration records
ppRecord :: Show a => a -> Text
ppRecord = T.toStrict . pShow
