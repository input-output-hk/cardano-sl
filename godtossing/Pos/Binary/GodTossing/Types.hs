-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class               (Cons (..), Field (..), deriveSimpleBi,
                                                 deriveSimpleBiCxt)
import           Pos.Core.Configuration         (HasConfiguration)
import           Pos.Core.Types                 (EpochIndex, EpochOrSlot, StakeholderId)
import           Pos.Core.Vss                   (VssCertificate, VssCertificatesMap)
import           Pos.Ssc.GodTossing.Core        (CommitmentsMap, Opening, OpeningsMap,
                                                 SharesMap, SignedCommitment)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData (VssCertData (..))

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

deriveSimpleBiCxt [t|HasConfiguration|] ''GtGlobalState [
    Cons 'GtGlobalState [
        Field [| _gsCommitments     :: CommitmentsMap |],
        Field [| _gsOpenings        :: OpeningsMap    |],
        Field [| _gsShares          :: SharesMap      |],
        Field [| _gsVssCertificates :: VssCertData    |]
    ]]

deriveSimpleBi ''GtSecretStorage [
    Cons 'GtSecretStorage [
        Field [| gssCommitment :: SignedCommitment |],
        Field [| gssOpening    :: Opening          |],
        Field [| gssEpoch      :: EpochIndex       |]
    ]]
