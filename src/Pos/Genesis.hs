-- | Blockchain genesis. Not to be confused with genesis block in epoch.

module Pos.Genesis
       (
       -- * Static state
         genesisAddresses
       , genesisKeyPairs
       , genesisPublicKeys
       , genesisSecretKeys
       , genesisUtxo
       , genesisUtxoPetty

       -- * Ssc
       , genesisLeaders
       ) where


import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import           Formatting         (int, sformat, (%))
import           Universum

import           Pos.Constants      (epochSlots, genesisN)
import           Pos.Crypto         (PublicKey, SecretKey, deterministicKeyGen,
                                     unsafeHash)
import           Pos.Types.Types    (Address (Address), SlotLeaders, TxOut (..), Utxo)


----------------------------------------------------------------------------
-- Static state
----------------------------------------------------------------------------

-- TODO get rid of this hardcode !!
-- Secret keys of genesis block participants shouldn't obviously be widely known
genesisKeyPairs :: [(PublicKey, SecretKey)]
genesisKeyPairs = map gen [0 .. genesisN - 1]
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
    M.fromList . take 3 $
    map (\a -> ((unsafeHash a, 0), TxOut a 10000)) genesisAddresses

-- | For every static stakeholder it generates `k` coins, but in `k`
-- transaction (1 coin each), where `k` is input parameter (depends on
-- node index).
--
-- Node 0 gets (k 0) coins one-by-one, everybody else (k
-- i) in one transaction.
genesisUtxoPetty :: (Int -> Int) -> Utxo
genesisUtxoPetty k =
    M.fromList $ flip concatMap (genesisAddresses `zip` [0..]) $ \(a,nodei) ->
        if nodei == 0
        then map (\i -> ((unsafeHash (show a ++ show i), 1), TxOut a 1)) [1..(k 0)]
        else [((unsafeHash a, (fromIntegral $ k nodei)), TxOut a 1)]

----------------------------------------------------------------------------
-- Slot leaders
----------------------------------------------------------------------------

genesisLeaders :: SlotLeaders
genesisLeaders = NE.fromList $ replicate epochSlots pk
  where
    pk =
        fromMaybe (panic "genesisPublicKeys is empty") $
        headMay genesisPublicKeys
