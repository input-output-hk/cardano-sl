-- | Helper functions to mock data.
-- |
-- | _Note_: All these functions are used for testing only.

module Explorer.Test.MockFactory where

import Prelude
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Data.Time.NominalDiffTime (NominalDiffTime, mkTime)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkCoin)
import Pos.Explorer.Web.ClientTypes (CAddressSummary(..), CBlockEntry(..), CHash(..), CTxEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CBlockEntry, cbeBlkHash, cbeEpoch, cbeSlot, cbeTimeIssued)

mkEmptyCTxEntry :: CTxEntry
mkEmptyCTxEntry = CTxEntry
    { cteId: mkCTxId "--"
    , cteTimeIssued: mkTime 0.0
    , cteAmount: mkCoin 0
    }

mkEmptyCAddressSummary :: CAddressSummary
mkEmptyCAddressSummary = CAddressSummary
    { caAddress: mkCAddress "--"
    , caTxNum: 0
    , caBalance: mkCoin 0
    , caTxList: []
    }

mkCBlockEntry :: CBlockEntry
mkCBlockEntry = CBlockEntry
    { cbeEpoch: 0
    , cbeSlot: 0
    , cbeBlkHash: mkCHash "0"
    , cbeTimeIssued: Nothing
    , cbeTxNum: 0
    , cbeTotalSent: mkCoin 0
    , cbeSize: 0
    , cbeRelayedBy: Nothing
    }


-- | Update time of a slot
setTimeOfBlock :: NominalDiffTime -> CBlockEntry -> CBlockEntry
setTimeOfBlock time block =
    set (_CBlockEntry <<< cbeTimeIssued) (Just time) block

-- | Update slot / epoch of a slot
setEpochSlotOfBlock :: Int -> Int -> CBlockEntry -> CBlockEntry
setEpochSlotOfBlock epoch slot block =
    set (_CBlockEntry <<< cbeEpoch) epoch $
    set (_CBlockEntry <<< cbeSlot) slot block

-- | Update hash of a slot
setHashOfBlock :: CHash -> CBlockEntry -> CBlockEntry
setHashOfBlock hash block =
    set (_CBlockEntry <<< cbeBlkHash) hash block
