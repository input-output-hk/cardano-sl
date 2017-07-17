-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashMap.Strict           as HM
import           Universum

import           Pos.Binary.Class              (Bi (..), enforceSize, encodeListLen, decodeListLen, matchSize)
import           Pos.Binary.Crypto             ()
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), Commitment (..),
                                                CommitmentsMap (..), GtPayload (..),
                                                GtProof (..), Opening (..),
                                                VssCertificate (..), mkCommitmentsMap,
                                                recreateVssCertificate)

instance Bi Commitment where
  encode Commitment{..} = encodeListLen 3 <> encode commShares
                                          <> encode commExtra
                                          <> encode commProof
  decode = do
    enforceSize "Commitment" 3
    commShares <- decode
    when (null commShares) $ fail "decode@Commitment: no shares"
    commExtra <- decode
    commProof <- decode
    return $ Commitment commExtra commProof commShares

instance Bi CommitmentsMap where
  encode = encode . HM.elems . getCommitmentsMap
  decode = mkCommitmentsMap <$> decode

instance Bi VssCertificate where
  encode vssCert = encodeListLen 4 <> encode (vcVssKey vssCert)
                                   <> encode (vcExpiryEpoch vssCert)
                                   <> encode (vcSignature vssCert)
                                   <> encode (vcSigningKey vssCert)
  decode = do
    enforceSize "VssCertificate" 4
    key <- decode
    epo <- decode
    sig <- decode
    sky <- decode
    case recreateVssCertificate key epo sig sky of
      Left e  -> fail e
      Right v -> pure v

instance Bi Opening where
  encode = encode . getOpening
  decode = Opening <$> decode

instance Bi GtPayload where
  encode input = case input of
    CommitmentsPayload  cmap vss -> encodeListLen 3 <> encode (0 :: Word8)
                                                    <> encode cmap <> encode vss
    OpeningsPayload     omap vss -> encodeListLen 3 <> encode (1 :: Word8)
                                                    <> encode omap <> encode vss
    SharesPayload       smap vss -> encodeListLen 3 <> encode (2 :: Word8)
                                                    <> encode smap <> encode vss
    CertificatesPayload vss      -> encodeListLen 2 <> encode (3 :: Word8)
                                                    <> encode vss
  decode = do
    len <- decodeListLen
    tag <- decode @Word8
    case tag of
      0 -> do
        matchSize len "GtPayload.CommitmentsPayload" 3
        CommitmentsPayload  <$> decode <*> decode
      1 -> do
        matchSize len "GtPayload.OpeningsPayload" 3
        OpeningsPayload     <$> decode <*> decode
      2 -> do
        matchSize len "GtPayload.SharesPayload" 3
        SharesPayload       <$> decode <*> decode
      3 -> do
        matchSize len "GtPayload.CertificatesPayload" 2
        CertificatesPayload <$> decode
      _ -> fail ("get@GtPayload: invalid tag: " ++ show tag)

instance Bi GtProof where
  encode input = case input of
    CommitmentsProof  cmap vss -> encodeListLen 3 <> encode (0 :: Word8)
                                                  <> encode cmap <> encode vss
    OpeningsProof     omap vss -> encodeListLen 3 <> encode (1 :: Word8)
                                                  <> encode omap <> encode vss
    SharesProof       smap vss -> encodeListLen 3 <> encode (2 :: Word8)
                                                  <> encode smap <> encode vss
    CertificatesProof vss      -> encodeListLen 2 <> encode (3 :: Word8)
                                                  <> encode vss
  decode = do
    len   <- decodeListLen
    tag <- decode @Word8
    case tag of
      0 -> do
        matchSize len "GtProof.CommitmentsProof" 3
        CommitmentsProof  <$> decode <*> decode
      1 -> do
        matchSize len "GtProof.OpeningsProof" 3
        OpeningsProof     <$> decode <*> decode
      2 -> do
        matchSize len "GtProof.SharesProof" 3
        SharesProof       <$> decode <*> decode
      3 -> do
        matchSize len "GtProof.CertificatesProof" 2
        CertificatesProof <$> decode
      _ -> fail ("decode@GtProof: invalid tag: " ++ show tag)
