-- | Higher-level functionality of LRC DB.

module Pos.DB.Lrc.Lrc
       ( prepareLrcDB
       ) where

import           Pos.DB.Class       (MonadDB)
import           Pos.DB.Lrc.Common  (prepareLrcCommon)
import           Pos.DB.Lrc.Leaders (prepareLrcLeaders)
import           Pos.DB.Lrc.Richmen (prepareLrcRichmen)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: MonadDB ssc m
    => m ()
prepareLrcDB = do
    prepareLrcLeaders
    prepareLrcRichmen
    prepareLrcCommon
