{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Genesis values related to GodTossing SSC.

module Pos.Ssc.GodTossing.Genesis
       ( genesisCertificates
#ifdef DEV_MODE
       , genesisVssKeyPairs
#endif
       ) where

import qualified Data.HashMap.Strict           as HM
import           Data.List                     (zipWith3)
import qualified Data.Text                     as T
import           Formatting                    (int, sformat, (%))
import           Universum

import           Pos.Constants                 (genesisN, vssMaxTTL, vssMinTTL)
import           Pos.Crypto                    (VssKeyPair, VssPublicKey,
                                                deterministicVssKeyGen, toVssPublicKey)
#ifdef DEV_MODE
import           Pos.Genesis                   (genesisKeyPairs)
#else
import           Pos.Genesis                   (compileGenData, gdVssCertificates)
#endif
import           Pos.Ssc.GodTossing.Core.Types (VssCertificatesMap, mkVssCertificate)
import           Pos.Types                     (EpochIndex (..))
import           Pos.Types.Address             (addressHash)
import           Pos.Util                      (asBinary)

#ifdef DEV_MODE
-- | List of 'VssKeyPair' in genesis.
genesisVssKeyPairs :: [VssKeyPair]
genesisVssKeyPairs = map gen [0 .. genesisN - 1]
  where
    gen :: Int -> VssKeyPair
    gen =
        deterministicVssKeyGen .
        encodeUtf8 .
        T.take 32 .
        sformat ("My awesome 32-byte seed :) #" %int % "             ")

-- | List of 'VssPublicKey' in genesis.
genesisVssPublicKeys :: [VssPublicKey]
genesisVssPublicKeys = map toVssPublicKey genesisVssKeyPairs

-- | Certificates in genesis represented as 'VssCertificatesMap'.
genesisCertificates :: VssCertificatesMap
genesisCertificates =
    case l of
        c0:c1:_:cs -> HM.fromList $ c0 : c1 : cs
        _          -> panic "genesisCertificates: can't happen"
  where
    l =
        zipWith3
            (\i (pk, sk) vssPk ->
                 ( addressHash pk
                 , mkVssCertificate sk (asBinary vssPk) $ ttlExp i))
            [0 :: Int ..]
            genesisKeyPairs
            genesisVssPublicKeys
    ttlExp 1 = EpochIndex vssMinTTL - 1
    ttlExp _ = vssMaxTTL - 1
#else
genesisCertificates :: VssCertificatesMap
genesisCertificates = gdVssCertificates compileGenData
#endif
