-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), Cons (..), Field (..), deriveSimpleBi, encodeListLen,
                                                   enforceSize)
import           Pos.Core                         (EpochIndex, HasCoreConstants)
import           Pos.Ssc.GodTossing.Core          (Opening, SignedCommitment,
                                                   VssCertificatesMap)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Ssc.GodTossing.Types         (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))

instance HasCoreConstants => Bi VssCertData where
    encode VssCertData{..} = encodeListLen 6 <>
        encode lastKnownEoS <>
        encode certs <>
        encode whenInsMap <>
        encode whenInsSet <>
        encode whenExpire <>
        encode expiredCerts
    decode = do
        enforceSize "VssCertData" 6
        VssCertData <$>
            decode <*>
            decode <*>
            decode <*>
            decode <*>
            decode <*>
            decode

instance HasCoreConstants => Bi GtGlobalState where
    encode GtGlobalState{..} = encodeListLen 4 <>
        encode _gsCommitments <>
        encode _gsOpenings <>
        encode _gsShares <>
        encode _gsVssCertificates
    decode = do
        enforceSize "GtGlobalState" 4
        GtGlobalState <$>
            decode <*>
            decode <*>
            decode <*>
            decode

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
