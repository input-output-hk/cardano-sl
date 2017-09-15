-- | Genesis values related to GodTossing SSC.

module Pos.Ssc.GodTossing.Genesis
       ( genesisCertificates
       , genesisDevVssKeyPairs
       ) where

import qualified Data.Text          as T
import           Formatting         (int, sformat, (%))
import           Universum

import           Pos.Core.Constants (genesisKeysN)
import           Pos.Core.Genesis   (genesisCertificates)
import           Pos.Crypto         (VssKeyPair, deterministicVssKeyGen)

-- | List of 'VssKeyPair's in genesis.
genesisDevVssKeyPairs :: [VssKeyPair]
genesisDevVssKeyPairs = map gen [0 .. genesisKeysN - 1]
  where
    gen :: Int -> VssKeyPair
    gen =
        deterministicVssKeyGen .
        encodeUtf8 .
        T.take 32 .
        sformat ("My awesome 32-byte seed :) #" %int % "             ")
