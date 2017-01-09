-- | Higher-level functionality of LRC DB.

module Pos.DB.Lrc.Lrc
       ( prepareLrcDB
       ) where

import           Pos.Context.Class  (WithNodeContext)
import           Pos.DB.Class       (MonadDB)
import           Pos.DB.Lrc.Common  (prepareLrcCommon)
import           Pos.DB.Lrc.Leaders (prepareLrcLeaders)
import           Pos.DB.Lrc.Richmen (SomeRichmenComponent, prepareLrcRichmen)
import           Pos.Types          (FullRichmenData)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: (WithNodeContext ssc m, MonadDB ssc m)
    => [(SomeRichmenComponent, FullRichmenData)] -> m ()
prepareLrcDB initialRichmen = do
    prepareLrcLeaders
    prepareLrcRichmen initialRichmen
    prepareLrcCommon
