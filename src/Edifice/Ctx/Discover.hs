-- |

module Edifice.Ctx.Discover where

import Util
import Art
import Edifice.Ctx.Types

runDiscover :: D ()
runDiscover = atError $ log "wahay, discoveries!"
