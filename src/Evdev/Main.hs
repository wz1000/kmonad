-- |

module Evdev.Main
  ( main
  , run
  )

where

import Preface

main :: OnlyIO ()
main = undefined

withEvdev :: UIO m => EvdevCfg -> (LinKeyEvent -> m a) -> m a
withEvdev = undefined
