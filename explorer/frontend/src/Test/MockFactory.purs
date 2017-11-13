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
    , cteTimeIssued: Just $ mkTime 0.0
    , cteAmount: mkCoin "0"
    }

-- | Update hash of a transcation
setIdOfTx :: CTxId -> CTxEntry -> CTxEntry
setIdOfTx txId tx =
    set (_CTxEntry <<< cteId) txId tx

-- | Update time of a transaction
setTimeOfTx :: NominalDiffTime -> CTxEntry -> CTxEntry
setTimeOfTx time tx =
    set (_CTxEntry <<< cteTimeIssued) (Just time) tx

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
    , cbeFees: mkCoin "0"
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
    let block' = setEpochOfBlock epoch block in
    set (_CBlockEntry <<< cbeSlot) slot block'

-- | Update epoch of a slot
setEpochOfBlock :: Int -> CBlockEntry -> CBlockEntry
setEpochOfBlock epoch block =
    set (_CBlockEntry <<< cbeEpoch) epoch block

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
    , ctbTimeIssued: Just $ mkTime 0.0
    , ctbInputs: mkCtbInputs [index]
    , ctbOutputs: mkCtbOutputs ((index + 1)..(index + 2))
    , ctbInputSum: mkCoin "0"
    , ctbOutputSum: mkCoin "0"
    }

mkCtbInput :: Int -> Int -> Maybe (Tuple CAddress CCoin)
mkCtbInput addr coin =
    Just $ Tuple (mkCAddress $ "address-" <> show addr) (mkCoin $ show coin)

mkCtbInputs :: Array Int -> Array (Maybe (Tuple CAddress CCoin))
mkCtbInputs indexes =
    map (\index -> mkCtbInput index index) indexes

mkCtbOutput :: Int -> Int -> Tuple CAddress CCoin
mkCtbOutput addr coin =
    Tuple (mkCAddress $ "address-" <> show addr) (mkCoin $ show coin)

mkCtbOutputs :: Array Int -> Array (Tuple CAddress CCoin)
mkCtbOutputs indexes =
    map (\index -> mkCtbOutput index index) indexes

mkCGenesisSummary :: CGenesisSummary
mkCGenesisSummary = CGenesisSummary
    { cgsNumTotal    : 4
    , cgsNumRedeemed : 3
    , cgsNumNotRedeemed: 1
    , cgsRedeemedAmountTotal: mkCoin "100"
    , cgsNonRedeemedAmountTotal: mkCoin "10"
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
