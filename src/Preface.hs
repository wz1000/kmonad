{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-|
Module      : Preface
Description : Code that will be imported into every module
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}

module Preface ( module X ) where

import Preface.External as X
import Preface.Types   as X
import Preface.Util    as X
