-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), label)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Ssc.GodTossing.Types         (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))

instance Bi GtGlobalState where
    put GtGlobalState {..} = do
        put _gsCommitments
        put _gsOpenings
        put _gsShares
        put _gsVssCertificates
    get = label "GtGlobalState" $ liftM4 GtGlobalState get get get get

instance Bi VssCertData where
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
    put (GtSecretStorage c o e) = put c >> put o >> put e
    get = label "GtSecretStorage" $
        GtSecretStorage <$> get <*> get <*> get

instance Bi GenesisGtData where
    put (GenesisGtData a) = put a
    get = label "GenesisGtData" $
        GenesisGtData <$> get
