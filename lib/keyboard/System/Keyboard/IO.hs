module System.Keyboard.IO where

import Prelude

import System.Keyboard.Types

import RIO.Partial (read, toEnum)
import qualified RIO.Text as T


-- | Read a 'Keycode' from Text.
--
readKeycode :: IsKeycode c => Text -> Maybe c
readKeycode "~" = Nothing
readKeycode t   = Just . view (from _Keycode) $ toEnum (read (unpack t) :: Int)

-- | Load a KeyTable from file by parsing a table
--
-- The file-format is extremely strict:
-- - The first 2 lines are ignored, leaving room for column names and a line
-- - Each line consists of 5 columns delineated by 7 '|' symbols
--
-- For an example, see 'kmonad:keycode_table/en_US.md'
loadKeyTable :: MonadIO m => FilePath -> m KeyTable
loadKeyTable f = do

  let g t = let [n, l, m, w, d] = map T.strip
                                . take 5 . drop 1
                                . T.split (== '|') $ t
            in KeyCongruence n d (readKeycode l) (readKeycode m) (readKeycode w)

  KeyTable . map g . drop 2 . T.lines <$> readFileUtf8 f
