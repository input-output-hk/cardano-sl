module Pos.Explorer.Web.Lenses.ClientTypes where

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Pos.Explorer.Web.ClientTypes
import Data.Maybe
import Data.Tuple
import Data.Time.NominalDiffTime (NominalDiffTime(..))
import Pos.Core.Types (Coin)


_CAddress :: Lens.Iso' CAddress String
_CAddress = Lens.iso unwrap CAddress
  where
    unwrap (CAddress x) = x

caAddress :: forall a b r. Lens.Lens { "caAddress" :: a | r } { "caAddress" :: b | r } a b
caAddress = Lens.lens _."caAddress" (_ { "caAddress" = _ })

caTxNum :: forall a b r. Lens.Lens { "caTxNum" :: a | r } { "caTxNum" :: b | r } a b
caTxNum = Lens.lens _."caTxNum" (_ { "caTxNum" = _ })

caBalance :: forall a b r. Lens.Lens { "caBalance" :: a | r } { "caBalance" :: b | r } a b
caBalance = Lens.lens _."caBalance" (_ { "caBalance" = _ })

caTxList :: forall a b r. Lens.Lens { "caTxList" :: a | r } { "caTxList" :: b | r } a b
caTxList = Lens.lens _."caTxList" (_ { "caTxList" = _ })

_CAddressSummary :: Lens.Iso' CAddressSummary
                      { "caAddress" :: CAddress
                      , "caTxNum" :: Int
                      , "caBalance" :: Coin
                      , "caTxList" :: Array CTxBrief
                      }
_CAddressSummary = Lens.iso unwrap CAddressSummary
  where
    unwrap (CAddressSummary x) = x

cbeEpoch :: forall a b r. Lens.Lens { "cbeEpoch" :: a | r } { "cbeEpoch" :: b | r } a b
cbeEpoch = Lens.lens _."cbeEpoch" (_ { "cbeEpoch" = _ })

cbeSlot :: forall a b r. Lens.Lens { "cbeSlot" :: a | r } { "cbeSlot" :: b | r } a b
cbeSlot = Lens.lens _."cbeSlot" (_ { "cbeSlot" = _ })

cbeBlkHash :: forall a b r. Lens.Lens { "cbeBlkHash" :: a | r } { "cbeBlkHash" :: b | r } a b
cbeBlkHash = Lens.lens _."cbeBlkHash" (_ { "cbeBlkHash" = _ })

cbeTimeIssued :: forall a b r. Lens.Lens { "cbeTimeIssued" :: a | r } { "cbeTimeIssued" :: b | r } a b
cbeTimeIssued = Lens.lens _."cbeTimeIssued" (_ { "cbeTimeIssued" = _ })

cbeTxNum :: forall a b r. Lens.Lens { "cbeTxNum" :: a | r } { "cbeTxNum" :: b | r } a b
cbeTxNum = Lens.lens _."cbeTxNum" (_ { "cbeTxNum" = _ })

cbeTotalSent :: forall a b r. Lens.Lens { "cbeTotalSent" :: a | r } { "cbeTotalSent" :: b | r } a b
cbeTotalSent = Lens.lens _."cbeTotalSent" (_ { "cbeTotalSent" = _ })

cbeSize :: forall a b r. Lens.Lens { "cbeSize" :: a | r } { "cbeSize" :: b | r } a b
cbeSize = Lens.lens _."cbeSize" (_ { "cbeSize" = _ })

cbeRelayedBy :: forall a b r. Lens.Lens { "cbeRelayedBy" :: a | r } { "cbeRelayedBy" :: b | r } a b
cbeRelayedBy = Lens.lens _."cbeRelayedBy" (_ { "cbeRelayedBy" = _ })

_CBlockEntry :: Lens.Iso' CBlockEntry
                  { "cbeEpoch" :: Int
                  , "cbeSlot" :: Int
                  , "cbeBlkHash" :: CHash
                  , "cbeTimeIssued" :: Maybe NominalDiffTime
                  , "cbeTxNum" :: Int
                  , "cbeTotalSent" :: Coin
                  , "cbeSize" :: Int
                  , "cbeRelayedBy" :: Maybe String
                  }
_CBlockEntry = Lens.iso unwrap CBlockEntry
  where
    unwrap (CBlockEntry x) = x

cbsEntry :: forall a b r. Lens.Lens { "cbsEntry" :: a | r } { "cbsEntry" :: b | r } a b
cbsEntry = Lens.lens _."cbsEntry" (_ { "cbsEntry" = _ })

cbsPrevHash :: forall a b r. Lens.Lens { "cbsPrevHash" :: a | r } { "cbsPrevHash" :: b | r } a b
cbsPrevHash = Lens.lens _."cbsPrevHash" (_ { "cbsPrevHash" = _ })

cbsNextHash :: forall a b r. Lens.Lens { "cbsNextHash" :: a | r } { "cbsNextHash" :: b | r } a b
cbsNextHash = Lens.lens _."cbsNextHash" (_ { "cbsNextHash" = _ })

cbsMerkleRoot :: forall a b r. Lens.Lens { "cbsMerkleRoot" :: a | r } { "cbsMerkleRoot" :: b | r } a b
cbsMerkleRoot = Lens.lens _."cbsMerkleRoot" (_ { "cbsMerkleRoot" = _ })

_CBlockSummary :: Lens.Iso' CBlockSummary
                    { "cbsEntry" :: CBlockEntry
                    , "cbsPrevHash" :: CHash
                    , "cbsNextHash" :: Maybe CHash
                    , "cbsMerkleRoot" :: CHash
                    }
_CBlockSummary = Lens.iso unwrap CBlockSummary
  where
    unwrap (CBlockSummary x) = x

_CHash :: Lens.Iso' CHash String
_CHash = Lens.iso unwrap CHash
  where
    unwrap (CHash x) = x

_CNetworkAddress :: Lens.Iso' CNetworkAddress String
_CNetworkAddress = Lens.iso unwrap CNetworkAddress
  where
    unwrap (CNetworkAddress x) = x

ctbId :: forall a b r. Lens.Lens { "ctbId" :: a | r } { "ctbId" :: b | r } a b
ctbId = Lens.lens _."ctbId" (_ { "ctbId" = _ })

ctbTimeIssued :: forall a b r. Lens.Lens { "ctbTimeIssued" :: a | r } { "ctbTimeIssued" :: b | r } a b
ctbTimeIssued = Lens.lens _."ctbTimeIssued" (_ { "ctbTimeIssued" = _ })

ctbInputs :: forall a b r. Lens.Lens { "ctbInputs" :: a | r } { "ctbInputs" :: b | r } a b
ctbInputs = Lens.lens _."ctbInputs" (_ { "ctbInputs" = _ })

ctbOutputs :: forall a b r. Lens.Lens { "ctbOutputs" :: a | r } { "ctbOutputs" :: b | r } a b
ctbOutputs = Lens.lens _."ctbOutputs" (_ { "ctbOutputs" = _ })

_CTxBrief :: Lens.Iso' CTxBrief
               { "ctbId" :: CTxId
               , "ctbTimeIssued" :: NominalDiffTime
               , "ctbInputs" :: Array (Tuple CAddress Coin)
               , "ctbOutputs" :: Array (Tuple CAddress Coin)
               }
_CTxBrief = Lens.iso unwrap CTxBrief
  where
    unwrap (CTxBrief x) = x

cteId :: forall a b r. Lens.Lens { "cteId" :: a | r } { "cteId" :: b | r } a b
cteId = Lens.lens _."cteId" (_ { "cteId" = _ })

cteTimeIssued :: forall a b r. Lens.Lens { "cteTimeIssued" :: a | r } { "cteTimeIssued" :: b | r } a b
cteTimeIssued = Lens.lens _."cteTimeIssued" (_ { "cteTimeIssued" = _ })

cteAmount :: forall a b r. Lens.Lens { "cteAmount" :: a | r } { "cteAmount" :: b | r } a b
cteAmount = Lens.lens _."cteAmount" (_ { "cteAmount" = _ })

_CTxEntry :: Lens.Iso' CTxEntry
               { "cteId" :: CTxId
               , "cteTimeIssued" :: NominalDiffTime
               , "cteAmount" :: Coin
               }
_CTxEntry = Lens.iso unwrap CTxEntry
  where
    unwrap (CTxEntry x) = x

_CTxId :: Lens.Iso' CTxId CHash
_CTxId = Lens.iso unwrap CTxId
  where
    unwrap (CTxId x) = x

ctsId :: forall a b r. Lens.Lens { "ctsId" :: a | r } { "ctsId" :: b | r } a b
ctsId = Lens.lens _."ctsId" (_ { "ctsId" = _ })

ctsTxTimeIssued :: forall a b r. Lens.Lens { "ctsTxTimeIssued" :: a | r } { "ctsTxTimeIssued" :: b | r } a b
ctsTxTimeIssued = Lens.lens _."ctsTxTimeIssued" (_ { "ctsTxTimeIssued" = _ })

ctsBlockTimeIssued :: forall a b r. Lens.Lens { "ctsBlockTimeIssued" :: a | r } { "ctsBlockTimeIssued" :: b | r } a b
ctsBlockTimeIssued = Lens.lens _."ctsBlockTimeIssued" (_ { "ctsBlockTimeIssued" = _ })

ctsBlockHeight :: forall a b r. Lens.Lens { "ctsBlockHeight" :: a | r } { "ctsBlockHeight" :: b | r } a b
ctsBlockHeight = Lens.lens _."ctsBlockHeight" (_ { "ctsBlockHeight" = _ })

ctsRelayedBy :: forall a b r. Lens.Lens { "ctsRelayedBy" :: a | r } { "ctsRelayedBy" :: b | r } a b
ctsRelayedBy = Lens.lens _."ctsRelayedBy" (_ { "ctsRelayedBy" = _ })

ctsTotalInput :: forall a b r. Lens.Lens { "ctsTotalInput" :: a | r } { "ctsTotalInput" :: b | r } a b
ctsTotalInput = Lens.lens _."ctsTotalInput" (_ { "ctsTotalInput" = _ })

ctsTotalOutput :: forall a b r. Lens.Lens { "ctsTotalOutput" :: a | r } { "ctsTotalOutput" :: b | r } a b
ctsTotalOutput = Lens.lens _."ctsTotalOutput" (_ { "ctsTotalOutput" = _ })

ctsFees :: forall a b r. Lens.Lens { "ctsFees" :: a | r } { "ctsFees" :: b | r } a b
ctsFees = Lens.lens _."ctsFees" (_ { "ctsFees" = _ })

ctsInputs :: forall a b r. Lens.Lens { "ctsInputs" :: a | r } { "ctsInputs" :: b | r } a b
ctsInputs = Lens.lens _."ctsInputs" (_ { "ctsInputs" = _ })

ctsOutputs :: forall a b r. Lens.Lens { "ctsOutputs" :: a | r } { "ctsOutputs" :: b | r } a b
ctsOutputs = Lens.lens _."ctsOutputs" (_ { "ctsOutputs" = _ })

_CTxSummary :: Lens.Iso' CTxSummary
                 { "ctsId" :: CTxId
                 , "ctsTxTimeIssued" :: NominalDiffTime
                 , "ctsBlockTimeIssued" :: Maybe NominalDiffTime
                 , "ctsBlockHeight" :: Maybe Int
                 , "ctsRelayedBy" :: Maybe CNetworkAddress
                 , "ctsTotalInput" :: Coin
                 , "ctsTotalOutput" :: Coin
                 , "ctsFees" :: Coin
                 , "ctsInputs" :: Array (Tuple CAddress Coin)
                 , "ctsOutputs" :: Array (Tuple CAddress Coin)
                 }
_CTxSummary = Lens.iso unwrap CTxSummary
  where
    unwrap (CTxSummary x) = x
