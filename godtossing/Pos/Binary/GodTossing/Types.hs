-- | Serialization of GodTossing types.

module Pos.Binary.GodTossing.Types () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), appendField, label, putField)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Ssc.GodTossing.Types         (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))

instance Bi GtGlobalState where
    sizeNPut = putField _gsCommitments
            <> putField _gsOpenings
            <> putField _gsShares
            <> putField _gsVssCertificates
    get = label "GtGlobalState" $ liftM4 GtGlobalState get get get get

instance Bi VssCertData where
    sizeNPut = putField lastKnownEoS
        `appendField` certs
        `appendField` certs
        `appendField` whenInsMap
        `appendField` whenInsSet
        `appendField` whenExpire
        `appendField` expiredCerts
    get = label "VssCertData" $
        VssCertData <$> get <*> get <*> get <*> get <*> get <*> get

instance Bi GtSecretStorage where
    sizeNPut = putField gssCommitment <> putField gssOpening <> putField gssEpoch
    get = label "GtSecretStorage" $
        GtSecretStorage <$> get <*> get <*> get

instance Bi GenesisGtData where
    sizeNPut = putField ggdVssCertificates
    get = label "GenesisGtData" $
        GenesisGtData <$> get
