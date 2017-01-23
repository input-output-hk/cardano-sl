-- | Higher-level functionality of LRC DB.

module Pos.DB.Lrc.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Pos.Context.Class  (WithNodeContext)
import           Pos.DB.Class       (MonadDB)
import           Pos.DB.Error       (DBError (..))
import           Pos.DB.Lrc.Common  (prepareLrcCommon)
import           Pos.DB.Lrc.Issuers (prepareLrcIssuers)
import           Pos.DB.Lrc.Leaders (prepareLrcLeaders)
import           Pos.DB.Lrc.Richmen (getRichmenUS, prepareLrcRichmen)
import           Pos.Util           (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: (WithNodeContext ssc m, MonadDB ssc m)
    => m ()
prepareLrcDB = do
    prepareLrcLeaders
    prepareLrcRichmen
    let cantReadErr =
            DBMalformed "Can't read richmen US after richem initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< getRichmenUS 0)
    prepareLrcIssuers totalStake
    prepareLrcCommon
