{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Serialization of core types from SSC.

module Pos.Binary.Core.Ssc
       (
       ) where

import           Universum

import qualified Data.HashSet as HS
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (Bi (..), Cons (..), Decoder, Encoding, Field (..),
                                   deriveSimpleBi, encodeListLen, enforceSize)
import           Pos.Binary.Core.Slotting ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Ssc.Types (Commitment (..), CommitmentsMap (..), Opening (..),
                                     OpeningsMap, SharesMap, SignedCommitment, SscPayload (..),
                                     SscProof (..), VssCertificatesHash, mkCommitmentsMap)
import           Pos.Core.Ssc.Vss (VssCertificate (..), VssCertificatesMap (..),
                                   mkVssCertificatesMap)
import           Pos.Crypto (Hash, PublicKey)
import           Pos.Util.Util (cborError)

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
        pure $ UnsafeVssCertificate key epo sig sky

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
    certs <- decode @(HashSet VssCertificate)
    let vssMap = mkVssCertificatesMap (toList certs)
    -- If the set is bigger than the map, then there must be some entires in
    -- the set which have the same signing key. That means it's a
    -- non-canonical encoding. The set itself could very well be canonical,
    -- though, since its values include more than just the signing keys.
    when (length certs > length vssMap) (cborError "duplicate vss key")
    pure vssMap

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

deriveSimpleBi ''SscPayload [
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
