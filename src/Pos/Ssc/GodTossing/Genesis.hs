-- | Genesis values related to Dynamic State SSC.

module Pos.Ssc.GodTossing.Genesis
       ( genesisVssKeyPairs
       , genesisVssPublicKeys
       , genesisCertificates
       ) where

import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import           Formatting                (int, sformat, (%))
import           Universum

import           Pos.Constants             (genesisN)
import           Pos.Crypto                (VssKeyPair, VssPublicKey,
                                            deterministicVssKeyGen, mkSigned,
                                            toVssPublicKey)
import           Pos.Genesis               (genesisKeyPairs)
import           Pos.Ssc.GodTossing.Base (VssCertificatesMap)

genesisVssKeyPairs :: [VssKeyPair]
genesisVssKeyPairs = map gen [0 .. genesisN - 1]
  where
    gen :: Int -> VssKeyPair
    gen =
        deterministicVssKeyGen .
        encodeUtf8 .
        T.take 32 .
        sformat ("My awesome 32-byte seed :) #" %int % "             ")

genesisVssPublicKeys :: [VssPublicKey]
genesisVssPublicKeys = map toVssPublicKey genesisVssKeyPairs

genesisCertificates :: VssCertificatesMap
genesisCertificates =
    HM.fromList $
    zipWith
        (\(pk, sk) vssPk -> (pk, mkSigned sk vssPk))
        genesisKeyPairs
        genesisVssPublicKeys
