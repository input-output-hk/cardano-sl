{-# LANGUAGE TypeFamilies #-}

-- | SSC-related utilities.

module Pos.Core.Ssc.Util
       (
         getCommShares
       , mkSscProof

       , checkSscPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Control.Lens (each, traverseOf)
import qualified Data.HashMap.Strict as HM

import           Pos.Binary.Class (Bi (..), fromBinary)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Ssc.Types (Commitment (..), CommitmentsMap, Opening, SscPayload (..),
                                     SscProof (..), VssCertificate, VssCertificatesMap (..))
import           Pos.Core.Ssc.Vss (checkVssCertificatesMap)
import           Pos.Crypto (EncShare, VssPublicKey, hash, HasCryptoConfiguration)

-- | Get commitment shares.
getCommShares :: Commitment -> Maybe [(VssPublicKey, NonEmpty EncShare)]
getCommShares =
    traverseOf (each . _1) (rightToMaybe . fromBinary) <=<      -- decode keys
    traverseOf (each . _2 . each) (rightToMaybe . fromBinary) . -- decode shares
    HM.toList . commShares

-- | Create proof (for inclusion into block header) from 'SscPayload'.
mkSscProof
    :: ( HasConfiguration
       , Bi VssCertificatesMap
       , Bi CommitmentsMap
       , Bi Opening
       , Bi VssCertificate
       ) => SscPayload -> SscProof
mkSscProof payload =
    case payload of
        CommitmentsPayload comms certs ->
            proof CommitmentsProof comms certs
        OpeningsPayload openings certs ->
            proof OpeningsProof openings certs
        SharesPayload shares certs     ->
            proof SharesProof shares certs
        CertificatesPayload certs      ->
            CertificatesProof (hash $ getVssCertificatesMap certs)
  where
    proof constr hm (getVssCertificatesMap -> certs) =
        constr (hash hm) (hash certs)

checkSscPayload
    :: ( HasCryptoConfiguration, MonadError Text m )
    => SscPayload
    -> m ()
checkSscPayload payload = checkVssCertificatesMap (spVss payload)
