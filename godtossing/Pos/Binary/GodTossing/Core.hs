-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashMap.Strict           as HM
import           Universum

import qualified Pos.Binary.Cbor               as Cbor
import           Pos.Binary.Class              (Bi (..), PokeWithSize, convertToSizeNPut,
                                                getWord8, label, labelS, putField, putS,
                                                putWord8S)
import           Pos.Binary.Crypto             ()
import           Pos.Core.Address              (addressHash)
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), Commitment (..),
                                                CommitmentsMap (..), GtPayload (..),
                                                GtProof (..), Opening (..),
                                                VssCertificate (..), mkCommitmentsMap,
                                                recreateVssCertificate)

instance Bi Commitment where
    sizeNPut = labelS "Commitment" $
           putField commShares
        <> putField commExtra
        <> putField commProof
    get = label "Commitment" $ do
        commShares <- get
        when (null commShares) $ fail "get@Commitment: no shares"
        commExtra <- get
        commProof <- get
        return Commitment {..}

instance Cbor.Bi Commitment where
  encode Commitment{..} = Cbor.encodeListLen 3 <> Cbor.encode commExtra
                                               <> Cbor.encode commProof
                                               <> Cbor.encode commShares
  decode = do
    Cbor.enforceSize "Commitment" 3
    Commitment <$> Cbor.decode <*> Cbor.decode <*> Cbor.decode

instance Bi CommitmentsMap where
    sizeNPut = labelS "CommitmentsMap" $ putField (toList . getCommitmentsMap)
    get = label "CommitmentsMap" $ mkCommitmentsMap <$> get

instance Cbor.Bi CommitmentsMap where
  encode = Cbor.encode . HM.elems . getCommitmentsMap
  decode = mkCommitmentsMap <$> Cbor.decode

instance Bi VssCertificate where
    sizeNPut = labelS "VssCertificate" $
        putField vcVssKey <>
        putField vcExpiryEpoch <>
        putField vcSignature <>
        putField vcSigningKey
    get = label "VssCertificate" $
        join $ liftM4 recreateVssCertificate get get get get

instance Cbor.Bi VssCertificate where
  encode vssCert = Cbor.encodeListLen 4 <> Cbor.encode (vcVssKey vssCert)
                                        <> Cbor.encode (vcExpiryEpoch vssCert)
                                        <> Cbor.encode (vcSignature vssCert)
                                        <> Cbor.encode (vcSigningKey vssCert)
  decode = do
    Cbor.enforceSize "VssCertificate" 4
    key <- Cbor.decode
    epo <- Cbor.decode
    sig <- Cbor.decode
    sky <- Cbor.decode
    case recreateVssCertificate key epo sig sky of
      Left e  -> fail e
      Right v -> pure v

instance Bi Opening where
    sizeNPut = labelS "Opening" $ putField getOpening
    get = label "Opening" $ Opening <$> get

instance Cbor.Bi Opening where
  encode = Cbor.encode . getOpening
  decode = Opening <$> Cbor.decode

instance Bi GtPayload where
    sizeNPut = labelS "GtPayload" $ convertToSizeNPut toBi
      where
        toBi :: GtPayload -> PokeWithSize ()
        toBi = \case
            CommitmentsPayload commMap vssMap ->
                putWord8S 0 <> putS commMap <> putS (toList vssMap)
            OpeningsPayload opMap vssMap ->
                putWord8S 1 <> putS opMap <> putS (toList vssMap)
            SharesPayload sharesMap vssMap ->
                putWord8S 2 <> putS sharesMap <> putS (toList vssMap)
            CertificatesPayload vssMap ->
                putWord8S 3 <> putS (toList vssMap)
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

instance Cbor.Bi GtPayload where
  encode input = case input of
    CommitmentsPayload  cmap vss -> Cbor.encodeListLen 3 <> Cbor.encode (0 :: Word8)
                                                         <> Cbor.encode cmap <> Cbor.encode vss
    OpeningsPayload     omap vss -> Cbor.encodeListLen 3 <> Cbor.encode (1 :: Word8)
                                                         <> Cbor.encode omap <> Cbor.encode vss
    SharesPayload       smap vss -> Cbor.encodeListLen 3 <> Cbor.encode (2 :: Word8)
                                                         <> Cbor.encode smap <> Cbor.encode vss
    CertificatesPayload vss      -> Cbor.encodeListLen 2 <> Cbor.encode (3 :: Word8)
                                                         <> Cbor.encode vss
  decode = do
    _   <- Cbor.decodeListLen
    tag <- Cbor.decode @Word8
    case tag of
      0 -> CommitmentsPayload  <$> Cbor.decode <*> Cbor.decode
      1 -> OpeningsPayload     <$> Cbor.decode <*> Cbor.decode
      2 -> SharesPayload       <$> Cbor.decode <*> Cbor.decode
      3 -> CertificatesPayload <$> Cbor.decode
      _ -> fail ("get@GtPayload: invalid tag: " ++ show tag)

instance Bi GtProof where
    sizeNPut = labelS "GtProof" $ convertToSizeNPut toBi
      where
        toBi :: GtProof -> PokeWithSize ()
        toBi = \case
            CommitmentsProof a b -> putWord8S 0 <> putS a <> putS b
            OpeningsProof a b    -> putWord8S 1 <> putS a <> putS b
            SharesProof a b      -> putWord8S 2 <> putS a <> putS b
            CertificatesProof a  -> putWord8S 3 <> putS a
    get = label "GtProof" $ do
        getWord8 >>= \case
            0 -> liftM2 CommitmentsProof get get
            1 -> liftM2 OpeningsProof get get
            2 -> liftM2 SharesProof get get
            3 -> CertificatesProof <$> get
            tag -> fail ("get@GtProof: invalid tag: " ++ show tag)

instance Cbor.Bi GtProof where
  encode input = case input of
    CommitmentsProof  cmap vss -> Cbor.encodeListLen 3 <> Cbor.encode (0 :: Word8)
                                                       <> Cbor.encode cmap <> Cbor.encode vss
    OpeningsProof     omap vss -> Cbor.encodeListLen 3 <> Cbor.encode (1 :: Word8)
                                                       <> Cbor.encode omap <> Cbor.encode vss
    SharesProof       smap vss -> Cbor.encodeListLen 3 <> Cbor.encode (2 :: Word8)
                                                       <> Cbor.encode smap <> Cbor.encode vss
    CertificatesProof vss      -> Cbor.encodeListLen 2 <> Cbor.encode (3 :: Word8)
                                                       <> Cbor.encode vss
  decode = do
    _   <- Cbor.decodeListLen
    tag <- Cbor.decode @Word8
    case tag of
      0 -> CommitmentsProof  <$> Cbor.decode <*> Cbor.decode
      1 -> OpeningsProof     <$> Cbor.decode <*> Cbor.decode
      2 -> SharesProof       <$> Cbor.decode <*> Cbor.decode
      3 -> CertificatesProof <$> Cbor.decode
      _ -> fail ("get@GtProof: invalid tag: " ++ show tag)
