-- | Serialization of core types from SSC.

module Pos.Binary.Core.Ssc
       (
       ) where

import           Universum

import qualified Data.HashSet as HS
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (Bi (..), Cons (..), Decoder, Encoding, Field (..),
                                   deriveSimpleBi, deriveSimpleBiCxt, encodeListLen, enforceSize)
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Ssc.Types (Commitment (..), CommitmentsMap (..), Opening (..),
                                     OpeningsMap, SharesMap, SignedCommitment, SscPayload (..),
                                     SscProof (..), VssCertificatesHash, mkCommitmentsMap)
import           Pos.Core.Ssc.Vss (VssCertificate (..), VssCertificatesMap (..),
                                   mkVssCertificatesMap, recreateVssCertificate)
import           Pos.Crypto (Hash, PublicKey)
import           Pos.Util.Util (cborError, toCborError)

instance Bi Commitment where
    encode Commitment{..} = encodeListLen 2 <> encode commShares
                                            <> encode commProof
    decode = do
        enforceSize "Commitment" 2
        commShares <- decode
        when (null commShares) $ cborError "decode@Commitment: no shares"
        commProof <- decode
        return $ Commitment commProof commShares

instance Bi CommitmentsMap where
    encode = encodeCommitments
    decode = decodeCommitments

instance HasConfiguration => Bi VssCertificate where
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
        toCborError $ recreateVssCertificate key epo sig sky

instance HasConfiguration => Bi VssCertificatesMap where
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

encodeVssCertificates :: HasConfiguration => VssCertificatesMap -> Encoding
encodeVssCertificates = encode . HS.fromList . toList

decodeVssCertificates :: HasConfiguration => Decoder s VssCertificatesMap
decodeVssCertificates = do
    certs <- toList <$> decode @(HashSet VssCertificate)
    -- If the attacker creates two certs that are different but have the
    -- same 'vcSigningKey', it's bad because then we lose canonicity (only
    -- one cert will be present in resulting map and the attacker can set
    -- the other cert to be anything at all). 'mkVssCertificatesMap' checks
    -- that all certificates have distinct keys, so we can safely use it.
    toCborError $ mkVssCertificatesMap certs

encodeCommitments :: CommitmentsMap -> Encoding
encodeCommitments = encode . HS.fromList . toList

decodeCommitments :: Decoder s CommitmentsMap
decodeCommitments = do
    comms <- toList <$> decode @(HashSet SignedCommitment)
    unless (allDistinct (map (view _1) comms :: [PublicKey])) $ cborError $
        "decodeCommitments: two commitments have the same signing key"
    pure (mkCommitmentsMap comms)

----------------------------------------------------------------------------
-- TH-generated instances go to the end of the file
----------------------------------------------------------------------------

deriveSimpleBiCxt [t|HasConfiguration|] ''SscPayload [
    Cons 'CommitmentsPayload [
        Field [| spComms    :: CommitmentsMap     |],
        Field [| spVss      :: VssCertificatesMap |] ],
    Cons 'OpeningsPayload [
        Field [| spOpenings :: OpeningsMap        |],
        Field [| spVss      :: VssCertificatesMap |] ],
    Cons 'SharesPayload [
        Field [| spShares   :: SharesMap          |],
        Field [| spVss      :: VssCertificatesMap |] ],
    Cons 'CertificatesPayload [
        Field [| spVss      :: VssCertificatesMap |] ]
    ]

deriveSimpleBi ''SscProof [
    Cons 'CommitmentsProof [
        Field [| sprComms    :: Hash CommitmentsMap |],
        Field [| sprVss      :: VssCertificatesHash |] ],
    Cons 'OpeningsProof [
        Field [| sprOpenings :: Hash OpeningsMap    |],
        Field [| sprVss      :: VssCertificatesHash |] ],
    Cons 'SharesProof [
        Field [| sprShares   :: Hash SharesMap      |],
        Field [| sprVss      :: VssCertificatesHash |] ],
    Cons 'CertificatesProof [
        Field [| sprVss      :: VssCertificatesHash |] ]
    ]
