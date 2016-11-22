{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( submitTxReal
       ) where

import           Universum

import           Pos.DHT               (DHTNode (dhtAddr), DHTNodeType (..),
                                        MonadDHT (..), filterByNodeType)
import           Pos.Launcher.Param    (NodeParams (..))
import           Pos.Launcher.Runner   (runRealMode)
import           Pos.Launcher.Runner   (bracketDHTInstance)
import           Pos.Launcher.Scenario (submitTx)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Statistics        (getNoStatsT)
import           Pos.Types             (Address, Coin, TxId)

-- | Submit tx in real mode.
submitTxReal
    :: forall ssc.
       SscConstraint ssc
    => NodeParams -> (TxId, Word32) -> (Address, Coin) -> IO ()
submitTxReal np input addrCoin = bracketDHTInstance (npBaseParams np) action
  where
    action inst = (runRealMode @ssc @()) inst np [] $ do
        peers <- getKnownPeers
        let na = dhtAddr <$> filterByNodeType DHTFull peers
        void $ getNoStatsT $ (submitTx @ssc) na input addrCoin
