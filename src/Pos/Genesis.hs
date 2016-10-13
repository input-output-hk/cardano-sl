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
       , genesisVssSecretKeys
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.Vector         as V
import           Universum

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (PublicKey, SecretKey, VssPublicKey, VssSecretKey,
                                      deterministicKeyGen, deterministicVssKeyGen,
                                      mkSigned, unsafeHash)
import           Pos.Types           (Address (Address), SlotLeaders, TxOut (..), Utxo,
                                      VssCertificatesMap)

----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

genesisKeyPairs :: [(PublicKey, SecretKey)]
genesisKeyPairs =
    [ fromMaybe
          (panic "deterministicKeyGen failed in Genesis")
          (deterministicKeyGen "Haskell HI Interactive Undo-Tree")
    ]

genesisSecretKeys :: [SecretKey]
genesisSecretKeys = map snd genesisKeyPairs

genesisPublicKeys :: [PublicKey]
genesisPublicKeys = map fst genesisKeyPairs

genesisAddresses :: [Address]
genesisAddresses = map Address genesisPublicKeys

genesisUtxo :: Utxo
genesisUtxo =
    M.fromList $ map (\a -> ((unsafeHash a, 0), TxOut a 100)) genesisAddresses

----------------------------------------------------------------------------
-- MPC, leaders
----------------------------------------------------------------------------

genesisVssKeyPairs :: [(VssPublicKey, VssSecretKey)]
genesisVssKeyPairs = [deterministicVssKeyGen mempty]

genesisVssSecretKeys :: [VssSecretKey]
genesisVssSecretKeys = map snd genesisVssKeyPairs

genesisVssPublicKeys :: [VssPublicKey]
genesisVssPublicKeys = map fst genesisVssKeyPairs

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
