-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashMap.Strict           as HM
import           Universum

import           Pos.Binary.Class              (Bi (..), PokeWithSize, Size (..),
                                                convertToSizeNPut, getSize, getWord8,
                                                label, pokeWithSize, putField, putWord8S)
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
    sizeNPut = convertToSizeNPut toBi
      where
        toBi :: GtPayload -> PokeWithSize ()
        toBi = \case
            CommitmentsPayload commMap vssMap ->
                putWord8S 0 <> pokeWithSize commMap <> pokeWithSize (toList vssMap)
            OpeningsPayload opMap vssMap ->
                putWord8S 1 <> pokeWithSize opMap <> pokeWithSize (toList vssMap)
            SharesPayload sharesMap vssMap ->
                putWord8S 2 <> pokeWithSize sharesMap <> pokeWithSize (toList vssMap)
            CertificatesPayload vssMap ->
                putWord8S 3 <> pokeWithSize (toList vssMap)
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
    sizeNPut = convertToSizeNPut toBi
      where
        toBi :: GtProof -> PokeWithSize ()
        toBi = \case
            CommitmentsProof a b -> putWord8S 0 <> pokeWithSize a <> pokeWithSize b
            OpeningsProof a b    -> putWord8S 1 <> pokeWithSize a <> pokeWithSize b
            SharesProof a b      -> putWord8S 2 <> pokeWithSize a <> pokeWithSize b
            CertificatesProof a  -> putWord8S 3 <> pokeWithSize a
    get = label "GtProof" $ do
        getWord8 >>= \case
            0 -> liftM2 CommitmentsProof get get
            1 -> liftM2 OpeningsProof get get
            2 -> liftM2 SharesProof get get
            3 -> CertificatesProof <$> get
            tag -> fail ("get@GtProof: invalid tag: " ++ show tag)
