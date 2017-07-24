-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashSet                  as HS
import           Universum

import           Pos.Binary.Class              (Bi (..), Decoder, Encoding, decodeListLen,
                                                encodeListLen, enforceSize, matchSize)
import           Pos.Binary.Crypto             ()
import           Pos.Crypto                    (PublicKey)
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), CommitmentsMap (..),
                                                GtPayload (..), GtProof (..),
                                                Opening (..), SignedCommitment,
                                                VssCertificate (..), VssCertificatesMap,
                                                mkCommitmentsMap, mkVssCertificatesMap,
                                                recreateVssCertificate)
import           Serokell.Util                 (allDistinct)

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
  encode = encodeCommitments
  decode = decodeCommitments

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
    CommitmentsPayload cmap vss ->
        encodeListLen 3
            <> encode (0 :: Word8)
            <> encode cmap
            <> encodeVssCertificates vss
    OpeningsPayload omap vss ->
        encodeListLen 3
            <> encode (1 :: Word8)
            <> encode omap
            <> encodeVssCertificates vss
    SharesPayload smap vss ->
        encodeListLen 3
            <> encode (2 :: Word8)
            <> encode smap
            <> encodeVssCertificates vss
    CertificatesPayload vss ->
        encodeListLen 2
            <> encode (3 :: Word8)
            <> encodeVssCertificates vss
  decode = do
    len <- decodeListLen
    tag <- decode @Word8
    case tag of
      0 -> do
        matchSize len "GtPayload.CommitmentsPayload" 3
        liftM2 CommitmentsPayload decode decodeVssCertificates
      1 -> do
        matchSize len "GtPayload.OpeningsPayload" 3
        liftM2 OpeningsPayload decode decodeVssCertificates
      2 -> do
        matchSize len "GtPayload.SharesPayload" 3
        liftM2 SharesPayload decode decodeVssCertificates
      3 -> do
        matchSize len "GtPayload.CertificatesPayload" 2
        CertificatesPayload <$> decodeVssCertificates
      _ -> fail ("decode@GtPayload: invalid tag: " <> show tag)

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

----------------------------------------------------------------------------
-- Maps encoding/decoding
----------------------------------------------------------------------------

{-
'VssCertificatesMap' and 'CommitmentsMap' are simply sets of values, indexed
by stakeholder id *for performance only*; the invariant is that the key
(stakeholder id) corresponds to the key stored in the value. This means that
the keys are redundant and putting them into encoded data is bad for two
reasons:

  * it takes more space
  * we have to do an extra invariant check after decoding

Instead, we serialize those maps as sets, and we make sure to check that
there are no values with duplicate stakeholder ids.
-}

encodeVssCertificates :: VssCertificatesMap -> Encoding
encodeVssCertificates = encode . HS.fromList . toList

decodeVssCertificates :: Decoder s VssCertificatesMap
decodeVssCertificates = do
    certs <- toList <$> decode @(HashSet VssCertificate)
    -- if the attacker creates two certs that are different but have the same
    -- 'vcSigningKey', it's bad because then we lose canonicity (only one
    -- cert will be present in resulting map and the attacker can set the
    -- other cert to be anything at all)
    unless (allDistinct (map vcSigningKey certs)) $
        fail "decodeVssCertificates: two certs have the same signing key"
    pure (mkVssCertificatesMap certs)

encodeCommitments :: CommitmentsMap -> Encoding
encodeCommitments = encode . HS.fromList . toList

decodeCommitments :: Decoder s CommitmentsMap
decodeCommitments = do
    comms <- toList <$> decode @(HashSet SignedCommitment)
    unless (allDistinct (map (view _1) comms :: [PublicKey])) $
        fail "decodeCommitments: two commitments have the same signing key"
    pure (mkCommitmentsMap comms)
