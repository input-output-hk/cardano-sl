{-# LANGUAGE ScopedTypeVariables #-}

-- | Base part of  /follow-the-satoshi/ procedure.

module Pos.Lrc.FollowTheSatoshiB
       ( followTheSatoshiM
       ) where

import           Data.List.NonEmpty (fromList)
import           Universum

import           Pos.Core.Coin      (coinToInteger, unsafeAddCoin)
import           Pos.Core.Constants (epochSlots)
import           Pos.Core.Types     (Coin, SharedSeed (..), StakeholderId, mkCoin)
import           Pos.Crypto         (deterministic, randomNumber)
import           Pos.Util.Iterator  (MonadIterator (..))

-- | A version of 'followTheSatoshi' that uses an iterator over 'TxOut's
-- instead of 'Utxo'.
followTheSatoshiM
    :: forall m . MonadIterator (StakeholderId, Coin) m
    => SharedSeed -> Coin -> m (NonEmpty StakeholderId)
followTheSatoshiM _ totalCoins
    | totalCoins == mkCoin 0 = panic "followTheSatoshiM: nobody has any stake"
followTheSatoshiM (SharedSeed seed) totalCoins = do
    res <- findLeaders (sortOn fst $ zip coinIndices [1..]) (mkCoin 0)
    pure . fromList . map fst . sortOn snd $ res
  where
    coinIndices :: [Coin]
    -- There won't be overflow because totalCoins fits in 64 bits
    coinIndices = map (mkCoin . fromInteger . (+1)) $
              deterministic seed $
              replicateM epochSlots (randomNumber (coinToInteger totalCoins))

    findLeaders
        :: [(Coin, Int)]
        -> Coin
        -> m [(StakeholderId, Int)]
    -- We found all coins we wanted to find
    findLeaders [] _ = pure []
    -- We ran out of items in the buffer so we take a new output
    -- and refill the buffer
    findLeaders coins sm = nextItem @(StakeholderId, Coin) >>=
        maybe (panic "followTheSatoshiM: indices out of range") (onItem coins)
      where
        -- We check whether `c` is covered by current item in the buffer
        onItem [] _ = pure []
        onItem (c:cs) it@(adr, val)
            | sm' >= fst c = ((adr, snd c):) <$> onItem cs it
            | otherwise = findLeaders (c:cs) sm'
          where
            sm' = unsafeAddCoin sm val
