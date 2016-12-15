{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Launcher
       ( runWalletMode
       , runWallet
       ) where

import           Universum

import           Pos.Communication     (allListeners)
import           Pos.DHT.Model         (mapListenerDHT)
import           Pos.DHT.Real          (KademliaDHTInstance)
import           Pos.Launcher          (NodeParams, addDevListeners, runNode,
                                        runRawRealMode)
import           Pos.Ssc.Class         (SscConstraint, SscParams)
import           Pos.Statistics        (NoStatsT (..))

import           Pos.Wallet.WalletMode (WalletRealMode)

-- | WalletMode runner.
runWalletMode
    :: forall ssc a.
       SscConstraint ssc
    => KademliaDHTInstance -> NodeParams -> SscParams ssc ->
       WalletRealMode ssc a -> IO a
runWalletMode inst np sscnp = runRawRealMode inst np sscnp listeners
  where
    listeners = addDevListeners @ssc np noStatsListeners
    noStatsListeners = map (mapListenerDHT getNoStatsT) (allListeners @ssc)

runWallet
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance
    -> [WalletRealMode ssc ()]
    -> NodeParams
    -> SscParams ssc
    -> IO ()
runWallet inst plugins np sscnp = runWalletMode inst np sscnp $
    getNoStatsT $ runNode @ssc $ map NoStatsT plugins
