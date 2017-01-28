-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.Ssc.GodTossing.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Binary.Crypto             ()
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), Opening (..),
                                                VssCertificate (..))

instance Bi Commitment where
    put Commitment {..} = do
        put commShares
        put commExtra
        put commProof
    get = do
        commShares <- get
        when (null commShares) $ fail "get@Commitment: no shares"
        commExtra <- get
        commProof <- get
        return Commitment {..}

instance Bi VssCertificate where
    put VssCertificate{..} = do
        put vcVssKey
        put vcExpiryEpoch
        put vcSignature
        put vcSigningKey
    get = liftM4 VssCertificate get get get get

instance Bi Opening where
    put (Opening secret) = put secret
    get = Opening <$> get
