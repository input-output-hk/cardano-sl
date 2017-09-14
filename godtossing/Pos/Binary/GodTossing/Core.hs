-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashSet                  as HS
import           Universum

import           Pos.Binary.Class              (Bi (..), Cons (..), Decoder, Encoding,
                                                Field (..), deriveSimpleBi, encodeListLen,
                                                enforceSize)
import           Pos.Binary.Crypto             ()
import           Pos.Crypto                    (Hash, PublicKey)
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), CommitmentsMap (..),
                                                GtPayload (..), GtProof (..),
                                                Opening (..), OpeningsMap, SharesMap,
                                                SignedCommitment, VssCertificate (..),
                                                VssCertificatesMap (..), mkCommitmentsMap,
                                                mkVssCertificatesMap,
                                                recreateVssCertificate)
import           Serokell.Util                 (allDistinct)

instance Bi Commitment where
  encode Commitment{..} = encodeListLen 2 <> encode commShares
                                          <> encode commProof
  decode = do
    enforceSize "Commitment" 2
    commShares <- decode
    when (null commShares) $ fail "decode@Commitment: no shares"
    commProof <- decode
    return $ Commitment commProof commShares

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

instance Bi VssCertificatesMap where
  encode = encodeVssCertificates
  decode = decodeVssCertificates

instance Bi Opening where
  encode = encode . getOpening
  decode = Opening <$> decode

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
    -- If the attacker creates two certs that are different but have the
    -- same 'vcSigningKey', it's bad because then we lose canonicity (only
    -- one cert will be present in resulting map and the attacker can set
    -- the other cert to be anything at all). 'mkVssCertificatesMap' checks
    -- that all certificates have distinct keys, so we can safely use it.
    mkVssCertificatesMap certs

encodeCommitments :: CommitmentsMap -> Encoding
encodeCommitments = encode . HS.fromList . toList

decodeCommitments :: Decoder s CommitmentsMap
decodeCommitments = do
    comms <- toList <$> decode @(HashSet SignedCommitment)
    unless (allDistinct (map (view _1) comms :: [PublicKey])) $
        fail "decodeCommitments: two commitments have the same signing key"
    pure (mkCommitmentsMap comms)

----------------------------------------------------------------------------
-- TH-generated instances go to the end of the file
----------------------------------------------------------------------------

deriveSimpleBi ''GtPayload [
    Cons 'CommitmentsPayload [
        Field [| gpComms    :: CommitmentsMap     |],
        Field [| gpVss      :: VssCertificatesMap |] ],
    Cons 'OpeningsPayload [
        Field [| gpOpenings :: OpeningsMap        |],
        Field [| gpVss      :: VssCertificatesMap |] ],
    Cons 'SharesPayload [
        Field [| gpShares   :: SharesMap          |],
        Field [| gpVss      :: VssCertificatesMap |] ],
    Cons 'CertificatesPayload [
        Field [| gpVss      :: VssCertificatesMap |] ]
    ]

deriveSimpleBi ''GtProof [
    Cons 'CommitmentsProof [
        Field [| gprComms    :: Hash CommitmentsMap     |],
        Field [| gprVss      :: Hash VssCertificatesMap |] ],
    Cons 'OpeningsProof [
        Field [| gprOpenings :: Hash OpeningsMap        |],
        Field [| gprVss      :: Hash VssCertificatesMap |] ],
    Cons 'SharesProof [
        Field [| gprShares   :: Hash SharesMap          |],
        Field [| gprVss      :: Hash VssCertificatesMap |] ],
    Cons 'CertificatesProof [
        Field [| gprVss      :: Hash VssCertificatesMap |] ]
    ]
