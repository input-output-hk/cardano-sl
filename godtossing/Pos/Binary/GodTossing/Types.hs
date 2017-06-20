-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), Cons (..), Field (..),
                                                   deriveSimpleBi, label, labelS,
                                                   putField)
import           Pos.Core.Types                   (EpochIndex)
import           Pos.Ssc.GodTossing.Core          (CommitmentsMap, Opening, OpeningsMap,
                                                   SharesMap, SignedCommitment,
                                                   VssCertificatesMap)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Ssc.GodTossing.Types         (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))

-- rewrite on deriveSimpleBi
instance Bi VssCertData where
    sizeNPut = labelS "VssCertData" $
        putField lastKnownEoS <>
        putField certs <>
        putField whenInsMap <>
        putField whenInsSet <>
        putField whenExpire <>
        putField expiredCerts
    get = label "VssCertData" $
        VssCertData <$> get <*> get <*> get <*> get <*> get <*> get

deriveSimpleBi ''GtGlobalState [
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

deriveSimpleBi ''GenesisGtData [
    Cons 'GenesisGtData [
        Field [| ggdVssCertificates :: VssCertificatesMap |]
    ]]
