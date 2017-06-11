-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashMap.Strict           as HM
import           Universum

import           Pos.Binary.Class              (Bi (..), Size (..), getSize, getWord8,
                                                label, putField, putWord8)
import           Pos.Binary.Crypto             ()
import           Pos.Core.Address              (addressHash)
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), Commitment (..),
                                                CommitmentsMap (..), GtPayload (..),
                                                GtProof (..), Opening (..),
                                                VssCertificate (..), mkCommitmentsMap,
                                                recreateVssCertificate)

instance Bi Commitment where
    sizeNPut = putField commShares
            <> putField commExtra
            <> putField commProof
    get = label "Commitment" $ do
        commShares <- get
        when (null commShares) $ fail "get@Commitment: no shares"
        commExtra <- get
        commProof <- get
        return Commitment {..}

instance Bi CommitmentsMap where
    sizeNPut = putField (toList . getCommitmentsMap)
    get = label "CommitmentsMap" $ mkCommitmentsMap <$> get

instance Bi VssCertificate where
    sizeNPut = putField vcVssKey
            <> putField vcExpiryEpoch
            <> putField vcSignature
            <> putField vcSignature
    get = label "VssCertificate" $
        join $ liftM4 recreateVssCertificate get get get get

instance Bi Opening where
    sizeNPut = putField getOpening
    get = label "Opening" $ Opening <$> get

instance Bi GtPayload where
    size = VarSize $ \case
        CommitmentsPayload a b -> 1 + getSize a + getSize (toList b)
        OpeningsPayload a b -> 1 + getSize a + getSize (toList b)
        SharesPayload a b -> 1 + getSize a + getSize (toList b)
        CertificatesPayload a -> 1 + getSize (toList a)
    put x =
        case x of
            CommitmentsPayload commMap vssMap ->
                putWord8 0 >> put commMap >> put (toList vssMap)
            OpeningsPayload opMap vssMap ->
                putWord8 1 >> put opMap >> put (toList vssMap)
            SharesPayload sharesMap vssMap ->
                putWord8 2 >> put sharesMap >> put (toList vssMap)
            CertificatesPayload vssMap -> putWord8 3 >> put (toList vssMap)
    get = label "GtPayload" $ do
        getWord8 >>= \case
            0 -> liftM2 CommitmentsPayload get getVssCerts
            1 -> liftM2 OpeningsPayload get getVssCerts
            2 -> liftM2 SharesPayload get getVssCerts
            3 -> CertificatesPayload <$> getVssCerts
            tag -> fail ("get@GtPayload: invalid tag: " ++ show tag)
          where
            getVssCerts = HM.fromList . map toCertPair <$> get
            toCertPair vc = (addressHash $ vcSigningKey vc, vc)

instance Bi GtProof where
    size = VarSize $ \case
        CommitmentsProof a b -> 1 + getSize a + getSize b
        OpeningsProof a b -> 1 + getSize a + getSize b
        SharesProof a b -> 1 + getSize a + getSize b
        CertificatesProof a -> 1 + getSize a
    put x = case x of
        CommitmentsProof a b -> putWord8 0 >> put a >> put b
        OpeningsProof a b    -> putWord8 1 >> put a >> put b
        SharesProof a b      -> putWord8 2 >> put a >> put b
        CertificatesProof a  -> putWord8 3 >> put a
    get = label "GtProof" $ do
        getWord8 >>= \case
            0 -> liftM2 CommitmentsProof get get
            1 -> liftM2 OpeningsProof get get
            2 -> liftM2 SharesProof get get
            3 -> CertificatesProof <$> get
            tag -> fail ("get@GtProof: invalid tag: " ++ show tag)
