-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), Size (..), combineSize, label,
                                                   sizeAddField, sizeOf)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Ssc.GodTossing.Types         (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))

instance Bi GtGlobalState where
    size = combineSize (_gsCommitments, _gsOpenings, _gsShares, _gsVssCertificates)
    put GtGlobalState {..} = do
        put _gsCommitments
        put _gsOpenings
        put _gsShares
        put _gsVssCertificates
    get = label "GtGlobalState" $ liftM4 GtGlobalState get get get get

instance Bi VssCertData where
    size = ConstSize 0
        `sizeAddField` lastKnownEoS
        `sizeAddField` certs
        `sizeAddField` whenInsMap
        `sizeAddField` whenInsSet
        `sizeAddField` whenExpire
        `sizeAddField` expiredCerts
    put VssCertData {..} = do
        put lastKnownEoS
        put certs
        put whenInsMap
        put whenInsSet
        put whenExpire
        put expiredCerts
    get = label "VssCertData" $
        VssCertData <$> get <*> get <*> get <*> get <*> get <*> get

instance Bi GtSecretStorage where
    size = combineSize (gssCommitment, gssOpening, gssEpoch)
    put (GtSecretStorage c o e) = put c >> put o >> put e
    get = label "GtSecretStorage" $
        GtSecretStorage <$> get <*> get <*> get

instance Bi GenesisGtData where
    size = sizeOf ggdVssCertificates
    put (GenesisGtData a) = put a
    get = label "GenesisGtData" $
        GenesisGtData <$> get
