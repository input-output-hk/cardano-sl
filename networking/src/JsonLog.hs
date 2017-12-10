{-|
Module      : JsonLog
Description : Logging to JSON files
License:      MIT
Maintainer:   lars.bruenjes@iohk.io
Stability:    experimental
Portability:  GHC

This module provides types and functions to support
logging to JSON files.
-}

module JsonLog
    ( module JsonLog.CanJsonLog
    , module JsonLog.Event
    , module JsonLog.JsonLogT
    ) where

import           JsonLog.CanJsonLog
import           JsonLog.Event
import           JsonLog.JsonLogT
