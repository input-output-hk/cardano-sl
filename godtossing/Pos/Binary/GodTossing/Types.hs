-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class                 (Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Binary.Cbor                  as Cbor
import           Pos.Core.Types                   (EpochIndex, EpochOrSlot, StakeholderId)
import           Pos.Ssc.GodTossing.Core          (CommitmentsMap, Opening, OpeningsMap,
                                                   SharesMap, SignedCommitment,
                                                   VssCertificate, VssCertificatesMap)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Ssc.GodTossing.Types         (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))

deriveSimpleBi ''VssCertData [
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

Cbor.deriveSimpleBi ''VssCertData [
    Cbor.Cons 'VssCertData [
        Cbor.Field [| lastKnownEoS :: EpochOrSlot                       |],
        Cbor.Field [| certs        :: VssCertificatesMap                |],
        Cbor.Field [| whenInsMap   :: HashMap StakeholderId EpochOrSlot |],
        Cbor.Field [| whenInsSet   :: Set (EpochOrSlot, StakeholderId)  |],
        Cbor.Field [| whenExpire   :: Set (EpochOrSlot, StakeholderId)  |],
        Cbor.Field [| expiredCerts :: Set (EpochOrSlot, (StakeholderId,
                                                      EpochOrSlot,
                                                      VssCertificate)) |]
    ]]

deriveSimpleBi ''GtGlobalState [
    Cons 'GtGlobalState [
        Field [| _gsCommitments     :: CommitmentsMap |],
        Field [| _gsOpenings        :: OpeningsMap    |],
        Field [| _gsShares          :: SharesMap      |],
        Field [| _gsVssCertificates :: VssCertData    |]
    ]]

Cbor.deriveSimpleBi ''GtGlobalState [
    Cbor.Cons 'GtGlobalState [
        Cbor.Field [| _gsCommitments     :: CommitmentsMap |],
        Cbor.Field [| _gsOpenings        :: OpeningsMap    |],
        Cbor.Field [| _gsShares          :: SharesMap      |],
        Cbor.Field [| _gsVssCertificates :: VssCertData    |]
    ]]

deriveSimpleBi ''GtSecretStorage [
    Cons 'GtSecretStorage [
        Field [| gssCommitment :: SignedCommitment |],
        Field [| gssOpening    :: Opening          |],
        Field [| gssEpoch      :: EpochIndex       |]
    ]]

Cbor.deriveSimpleBi ''GtSecretStorage [
    Cbor.Cons 'GtSecretStorage [
        Cbor.Field [| gssCommitment :: SignedCommitment |],
        Cbor.Field [| gssOpening    :: Opening          |],
        Cbor.Field [| gssEpoch      :: EpochIndex       |]
    ]]

deriveSimpleBi ''GenesisGtData [
    Cons 'GenesisGtData [
        Field [| ggdVssCertificates :: VssCertificatesMap |]
    ]]

Cbor.deriveSimpleBi ''GenesisGtData [
    Cbor.Cons 'GenesisGtData [
        Cbor.Field [| ggdVssCertificates :: VssCertificatesMap |]
    ]]
