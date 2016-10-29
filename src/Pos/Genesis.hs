-- | Blockchain genesis. Not to be confused with genesis block in epoch.

module Pos.Genesis
       (
       -- * Static state
         genesisAddresses
       , genesisKeyPairs
       , genesisPublicKeys
       , genesisSecretKeys
       , genesisUtxo

       -- * MPC
       , genesisCertificates
       , genesisLeaders
       , genesisVssKeyPairs
       , genesisVssPublicKeys
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Formatting          (int, sformat, (%))
import           Universum

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (PublicKey, SecretKey, VssKeyPair,
                                      VssPublicKey, deterministicKeyGen,
                                      deterministicVssKeyGen, mkSigned,
                                      toVssPublicKey, unsafeHash)
import           Pos.Types           (Address (Address), SlotLeaders,
                                      TxOut (..), Utxo, VssCertificatesMap)

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

-- TODO get rid of this hardcode !!
-- Secret keys of genesis block participants shouldn't obviously be widely known
genesisKeyPairs :: [(PublicKey, SecretKey)]
genesisKeyPairs = map gen [0 .. 41]
  where
    gen :: Int -> (PublicKey, SecretKey)
    gen =
        fromMaybe (panic "deterministicKeyGen failed in Genesis") .
        deterministicKeyGen .
        encodeUtf8 .
        T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

genesisSecretKeys :: [SecretKey]
genesisSecretKeys = map snd genesisKeyPairs

genesisPublicKeys :: [PublicKey]
genesisPublicKeys = map fst genesisKeyPairs

genesisAddresses :: [Address]
genesisAddresses = map Address genesisPublicKeys

genesisUtxo :: Utxo
genesisUtxo =
    M.fromList $ map (\a -> ((unsafeHash a, 0), TxOut a 10000)) genesisAddresses

----------------------------------------------------------------------------
-- MPC, leaders
----------------------------------------------------------------------------

genesisVssKeyPairs :: [VssKeyPair]
genesisVssKeyPairs = map gen [0 .. 42]
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

genesisLeaders :: SlotLeaders
genesisLeaders = V.replicate epochSlots pk
  where
    pk =
        fromMaybe (panic "genesisPublicKeys is empty") $
        headMay genesisPublicKeys
