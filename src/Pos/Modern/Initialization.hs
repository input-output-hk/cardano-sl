module Pos.Modern.Initialization
       (
         loadInitDataFromDB
       ) where

import           Universum

import           Pos.Modern.DB.Class (MonadDB (..))
import           Pos.Ssc.Class       (SscGlobalState)
import           Pos.Types           (HeaderHash)

loadInitDataFromDB :: MonadDB ssc m => m (HeaderHash ssc, SscGlobalState ssc)
loadInitDataFromDB = notImplemented
