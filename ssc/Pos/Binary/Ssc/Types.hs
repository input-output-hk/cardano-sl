-- | Serialization of SSC types.

module Pos.Binary.Ssc.Types () where

import           Universum

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi, deriveSimpleBiCxt)
import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting (EpochIndex, EpochOrSlot)
import           Pos.Core.Ssc (CommitmentsMap, Opening, OpeningsMap, SharesMap, SignedCommitment,
                               VssCertificate, VssCertificatesMap)
import           Pos.Ssc.Types (SscGlobalState (..), SscSecretStorage (..))
import           Pos.Ssc.VssCertData (VssCertData (..))

deriveSimpleBiCxt [t|HasConfiguration|] ''VssCertData [
    Cons 'VssCertData [
        Field [| lastKnownEoS :: EpochOrSlot                       |],
        Field [| certs        :: VssCertificatesMap                |],
        Field [| whenInsMap   :: HashMap StakeholderId EpochOrSlot |],
        Field [| whenInsSet   :: Set (EpochOrSlot, StakeholderId)  |],
        Field [| whenExpire   :: Set (EpochOrSlot, StakeholderId)  |],
        Field [| expiredCerts :: Set (EpochOrSlot, (StakeholderId,
                                                      EpochOrSlot,
                                                      VssCertificate)) |]
    ]]

deriveSimpleBiCxt [t|HasConfiguration|] ''SscGlobalState [
    Cons 'SscGlobalState [
        Field [| _sgsCommitments     :: CommitmentsMap |],
        Field [| _sgsOpenings        :: OpeningsMap    |],
        Field [| _sgsShares          :: SharesMap      |],
        Field [| _sgsVssCertificates :: VssCertData    |]
    ]]

deriveSimpleBi ''SscSecretStorage [
    Cons 'SscSecretStorage [
        Field [| sssCommitment :: SignedCommitment |],
        Field [| sssOpening    :: Opening          |],
        Field [| sssEpoch      :: EpochIndex       |]
    ]]
