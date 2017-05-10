-- | Higher-level functionality of LRC DB.

module Pos.Lrc.DB.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import qualified Ether

import           Pos.Context.Context (GenesisLeaders, GenesisUtxo)
import           Pos.DB.Class        (MonadDB)
import           Pos.DB.Error        (DBError (..))
import           Pos.Lrc.DB.Common   (prepareLrcCommon)
import           Pos.Lrc.DB.Issuers  (prepareLrcIssuers)
import           Pos.Lrc.DB.Leaders  (prepareLrcLeaders)
import           Pos.Lrc.DB.Richmen  (getRichmenUS, prepareLrcRichmen)
import           Pos.Lrc.DB.Seed     (prepareLrcSeed)
import           Pos.Util            (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: ( Ether.MonadReader' GenesisLeaders m
       , Ether.MonadReader' GenesisUtxo m
       , MonadDB m )
    => m ()
prepareLrcDB = do
    prepareLrcLeaders
    prepareLrcRichmen
    let cantReadErr =
            DBMalformed "Can't read richmen US after richem initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< getRichmenUS 0)
    prepareLrcIssuers totalStake
    prepareLrcSeed
    prepareLrcCommon
