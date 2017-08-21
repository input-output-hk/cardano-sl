-- | Helper for core types.

module Pos.Txp.Core.Core
       ( addrBelongsTo
       , addrBelongsToSet
       , emptyTxPayload
       , flattenTxPayload
       , mkTxProof
       , txInToPair
       , txOutStake
       ) where

import           Universum

import           Control.Lens        (ix)
import qualified Data.Hashable       as Hashable (hash)
import qualified Data.HashSet        as HS
import           Data.List           (zipWith)
import qualified Data.Map.Strict     as M

import           Pos.Binary.Core     ()
import           Pos.Binary.Crypto   ()
import           Pos.Binary.Txp.Core ()
import           Pos.Core            (AddrStakeDistribution (..), Address (..), Coin,
                                      CoinPortion, StakeholderId, StakesList,
                                      aaStakeDistribution, addrAttributesUnwrapped,
                                      applyCoinPortionDown, coinToInteger, mkCoin,
                                      sumCoins, unsafeAddCoin, unsafeGetCoin,
                                      unsafeIntegerToCoin)
import           Pos.Core.Genesis    (GenesisWStakeholders (..), bootDustThreshold)
import           Pos.Crypto          (hash)
import           Pos.Merkle          (mtRoot)
import           Pos.Txp.Core.Types  (TxAux (..), TxId, TxIn (..), TxOut (..),
                                      TxOutAux (..), TxPayload (..), TxProof (..),
                                      mkTxPayload)

-- | A predicate for `TxOutAux` which checks whether given address
-- belongs to it.
addrBelongsTo :: TxOutAux -> Address -> Bool
addrBelongsTo TxOutAux {..} = (== txOutAddress toaOut)

-- | Extended version of `addBelongsTo`, allows to compare against several
-- addresses.
addrBelongsToSet :: TxOutAux -> HashSet Address -> Bool
TxOutAux {..} `addrBelongsToSet` addrs = txOutAddress toaOut `HS.member` addrs

-- | Make a pair from 'TxIn'.
txInToPair :: TxIn -> (TxId, Word32)
txInToPair (TxIn h i) = (h, i)

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
txOutStake :: GenesisWStakeholders -> TxOut -> StakesList
txOutStake genStakeholders TxOut {..} =
    case aaStakeDistribution (addrAttributesUnwrapped txOutAddress) of
        BootstrapEraDistr            -> bootstrapEraDistr genStakeholders txOutValue
        SingleKeyDistr sId           -> [(sId, txOutValue)]
        UnsafeMultiKeyDistr distrMap -> computeMultiKeyDistr (M.toList distrMap)
  where
    outValueInteger = coinToInteger txOutValue
    -- For all stakeholders in this list (which is 'Ord'-ered) except
    -- the first one we use 'applyCoinPortionDown'. For the first one
    -- we take the difference. It is safe because sum of portions must
    -- be 1.
    computeMultiKeyDistr :: [(StakeholderId, CoinPortion)] -> StakesList
    computeMultiKeyDistr [] =
        error $ "txOutStake: impossible happened, " <>
        "multi key distribution is empty"
    computeMultiKeyDistr ((headStakeholder, _):rest) =
        let restDistr = computeMultiKeyDistrRest rest
            restDistrSum = sumCoins $ map snd restDistr
            headStake = outValueInteger - restDistrSum
        -- 'unsafeIntegerToCoin' is safe by construction
        in (headStakeholder, unsafeIntegerToCoin headStake) : restDistr
    computeMultiKeyDistrRest :: [(StakeholderId, CoinPortion)] -> StakesList
    computeMultiKeyDistrRest = map (second (`applyCoinPortionDown` txOutValue))

-- | Construct 'TxProof' which proves given 'TxPayload'.
mkTxProof :: TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot _txpTxs
    , txpWitnessesHash = hash _txpWitnesses
    }

-- | Convert 'TxPayload' into a flat list of `TxAux`s.
flattenTxPayload :: TxPayload -> [TxAux]
flattenTxPayload UnsafeTxPayload {..} =
    zipWith TxAux (toList _txpTxs) _txpWitnesses

emptyTxPayload :: TxPayload
emptyTxPayload = mkTxPayload []

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

-- Compute bootstrap era distribution.
--
-- This function splits coin into chunks of @weightsSum@ and
-- distributes them between bootstrap era stakeholders. Remainder is
-- assigned randomly (among bootstrap era stakeholders) based on hash of the
-- coin.
-- TODO CSL-1489 Don't use 'hash' which is used here!
--
-- If coin is lower than 'bootDustThreshold' then this function
-- distributes coins among first stakeholders in the list according to
-- their weights. The list is ordered by 'StakeholderId's.
bootstrapEraDistr ::
       GenesisWStakeholders -> Coin -> [(StakeholderId, Coin)]
bootstrapEraDistr g@(GenesisWStakeholders bootWHolders) c
    | c < bootDustThreshold g =
          snd $ foldr foldrFunc (0::Word64,[]) (M.toList bootWHolders)
    | otherwise =
          bootHolders `zip` stakeCoins
  where
    foldrFunc (s,w) r@(totalSum, res) = case compare totalSum cval of
        EQ -> r
        GT -> error "genesisSplitBoot: totalSum > cval can't happen"
        LT -> let w' = (fromIntegral w :: Word64)
                  toInclude = bool w' (cval - totalSum) (totalSum + w' > cval)
              in (totalSum + toInclude
                 ,(s, mkCoin toInclude):res)

    weights :: [Word64]
    weights = map fromIntegral $ toList bootWHolders

    weightsSum :: Word64
    weightsSum = sum weights

    bootHolders :: [StakeholderId]
    bootHolders = M.keys bootWHolders

    (d :: Word64, m :: Word64) = divMod cval weightsSum

    -- Person who will get the remainder.
    remReceiver :: Int
    remReceiver = abs (Hashable.hash c) `mod` addrsNum

    cval :: Word64
    cval = unsafeGetCoin c

    addrsNum :: Int
    addrsNum = length bootHolders

    stakeCoins :: [Coin]
    stakeCoins =
        map (\w -> mkCoin $ w * d) weights &
        ix remReceiver %~ unsafeAddCoin (mkCoin m)
