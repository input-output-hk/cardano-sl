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
import qualified Data.Vector         as V
import           Universum

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (PublicKey, SecretKey, VssKeyPair, VssPublicKey,
                                      deterministicKeyGen, deterministicVssKeyGen,
                                      mkSigned, toVssPublicKey, unsafeHash)
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

genesisVssKeyPairs :: [(VssKeyPair)]
genesisVssKeyPairs = [deterministicVssKeyGen mempty]

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
