-- | Higher-level functionality of LRC DB.

module Pos.Lrc.DB.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Pos.Context.Class  (WithNodeContext)
import           Pos.DB.Class       (MonadDB)
import           Pos.DB.Error       (DBError (..))
import           Pos.Lrc.DB.Common  (prepareLrcCommon)
import           Pos.Lrc.DB.Issuers (prepareLrcIssuers)
import           Pos.Lrc.DB.Leaders (prepareLrcLeaders)
import           Pos.Lrc.DB.Richmen (getRichmenUS, prepareLrcRichmen)
import           Pos.Util           (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: (WithNodeContext ssc m, MonadDB m)
    => m ()
prepareLrcDB = do
    prepareLrcLeaders
    prepareLrcRichmen
    let cantReadErr =
            DBMalformed "Can't read richmen US after richem initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< getRichmenUS 0)
    prepareLrcIssuers totalStake
    prepareLrcCommon
