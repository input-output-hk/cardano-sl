-- | Helper functions to mock data.
-- |
-- | _Note_: All these functions are used for testing only.

module Explorer.Test.MockFactory where

import Prelude

import Data.Array ((..))
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Data.Time.NominalDiffTime (NominalDiffTime, mkTime)
import Data.Tuple (Tuple(..))
import Explorer.Types.State (CTxBriefs)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkCoin)
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary(..), CAddressType(..), CBlockEntry(..), CCoin, CGenesisAddressInfo(..), CGenesisSummary(..), CHash, CTxBrief(..), CTxEntry(..), CTxId)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddressSummary, _CBlockEntry, _CTxEntry, caTxList, cbeBlkHash, cbeEpoch, cbeSlot, cbeTimeIssued, cteId, cteTimeIssued)

-- | Creates a `CTxEntry` with "empty" data
mkEmptyCTxEntry :: CTxEntry
mkEmptyCTxEntry = CTxEntry
    { cteId: mkCTxId "--"
    , cteTimeIssued: mkTime 0.0
    , cteAmount: mkCoin "0"
    }

-- | Update hash of a transcation
setIdOfTx :: CTxId -> CTxEntry -> CTxEntry
setIdOfTx txId tx =
    set (_CTxEntry <<< cteId) txId tx

-- | Update time of a transaction
setTimeOfTx :: NominalDiffTime -> CTxEntry -> CTxEntry
setTimeOfTx time tx =
    set (_CTxEntry <<< cteTimeIssued) time tx

mkEmptyCAddressSummary :: CAddressSummary
mkEmptyCAddressSummary = CAddressSummary
    { caAddress: mkCAddress "--"
    , caType: CUnknownAddress
    , caTxNum: 0
    , caBalance: mkCoin "0"
    , caTxList: []
    }

-- | Update txs of a `CAddressSummary`
setTxOfAddressSummary :: CTxBriefs -> CAddressSummary -> CAddressSummary
setTxOfAddressSummary txs addr =
    set (_CAddressSummary <<< caTxList) txs addr

mkCBlockEntry :: CBlockEntry
mkCBlockEntry = CBlockEntry
    { cbeEpoch: 0
    , cbeSlot: 0
    , cbeBlkHash: mkCHash "0"
    , cbeTimeIssued: Nothing
    , cbeTxNum: 0
    , cbeTotalSent: mkCoin "0"
    , cbeSize: 0
    , cbeBlockLead: Nothing
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

mkCTxBriefs :: Array Int -> CTxBriefs
mkCTxBriefs indexes =
    map mkCTxBrief indexes

mkCTxBrief :: Int -> CTxBrief
mkCTxBrief index = CTxBrief
    { ctbId: mkCTxId $ show index
    , ctbTimeIssued: mkTime 0.0
    , ctbInputs: mkCtbInOutputs [index]
    , ctbOutputs: mkCtbInOutputs ((index + 1)..(index + 2))
    , ctbInputSum: mkCoin "0"
    , ctbOutputSum: mkCoin "0"
    }

mkCtbInOutput :: Int -> Int -> Tuple CAddress CCoin
mkCtbInOutput addr coin =
    Tuple (mkCAddress $ "address-" <> show addr) (mkCoin $ show coin)

mkCtbInOutputs :: Array Int -> Array (Tuple CAddress CCoin)
mkCtbInOutputs indexes =
    map (\index -> mkCtbInOutput index index) indexes

mkCGenesisSummary :: CGenesisSummary
mkCGenesisSummary = CGenesisSummary
    { cgsNumTotal    : 2
    , cgsNumRedeemed : 1
    }

mkCGenesisAddressInfo :: CGenesisAddressInfo
mkCGenesisAddressInfo =
    -- Commenting out RSCoin addresses until they can actually be displayed.
    -- See comment in src/Pos/Explorer/Web/ClientTypes.hs for more information.
    CGenesisAddressInfo
    { cgaiCardanoAddress : mkCAddress "3meLwrCDE4C7RofEdkZbUuR75ep3EcTmZv9ebcdjfMtv5H"
    -- , cgaiRSCoinAddress  : mkCAddress "JwvXUQ31cvrFpqqtx6fB-NOp0Q-eGQs74yXMGa-72Ak="
    , cgaiGenesisAmount  : mkCoin "15000000"
    , cgaiIsRedeemed     : false
    }
