{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.FollowTheSatoshi
       ( followTheSatoshi
       , followTheSatoshiM
       ) where




import           Control.Lens        ((.=), _1, _2)
import           Control.Monad.State (get)
import           Data.List.NonEmpty  (NonEmpty, fromList)
import           Universum

import           Pos.Constants       (epochSlots)
import           Pos.Crypto          (deterministic, randomNumber)
import           Pos.Modern.Iterator (ListHolder, MonadIterator (..), runListHolder)
import           Pos.Types.Types     (Address, Coin (..), SharedSeed (..), TxOut (..),
                                      Utxo)


followTheSatoshiM :: forall m . MonadIterator m TxOut
                  => SharedSeed -> Coin -> m (NonEmpty Address)
followTheSatoshiM (SharedSeed seed) totalCoins = do
    let coinIndices :: [Coin]
        coinIndices = map (fromInteger . (+1)) $
                  deterministic seed $
                  replicateM epochSlots (randomNumber (toInteger totalCoins))

        findLeaders :: StateT ([(Coin, Int)], Coin) m [(Address, Int)]
        findLeaders = do
            (coinIndx, sm) <- get
            case coinIndx of
                []     -> pure []
                (c:cs) ->
                    maybe (panic "followTheSatoshi: indices out of range")
                    (\TxOut{..} -> do
                        if sm + txOutValue >= fst c then do
                            _1 .= cs
                            ((txOutAddress, snd c):) <$> findLeaders
                        else do
                            _2 .= sm + txOutValue -- is lens usage dangerous for perfomance?
                            _ <- nextItem @_ @TxOut
                            findLeaders) =<< curItem
    res <- evalStateT findLeaders (sortOn fst $ zip coinIndices [1..], 0::Coin)
    pure . fromList . map fst . sortOn snd $ res

-- | Choose several random stakeholders (specifically, their amount is
-- currently hardcoded in 'Pos.Constants.epochSlots').
--
-- The probability that a stakeholder will be chosen is proportional to the
-- number of coins this stakeholder holds. The same stakeholder can be picked
-- more than once.
--
-- How the algorithm works: we sort all unspent outputs in a deterministic
-- way (lexicographically) and have an ordered sequence of pairs @(Address,
-- Coin)@. Then we choose several random 'i's between 1 and amount of satoshi
-- in the system; to find owner of 'i'th coin we find the lowest x such that
-- sum of all coins in this list up to 'i'th is not less than 'i' (and then
-- 'x'th address is the owner).
followTheSatoshi :: SharedSeed -> Utxo -> NonEmpty Address
followTheSatoshi seed utxo
    | null outputs = panic "followTheSatoshi: utxo is empty"
    | otherwise    = runListHolder (followTheSatoshiM @(ListHolder TxOut) seed totalCoins) outputs
  where
    outputs = toList utxo

    totalCoins = sum (map txOutValue outputs)
