{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Wallet.Launcher
       ( runWalletMode
       , runWallet
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Pos.Communication      (allListeners)
import           Pos.DHT.Model          (mapListenerDHT)
import           Pos.DHT.Real           (KademliaDHTInstance)
import           Pos.Launcher           (NodeParams, addDevListeners, runNode,
                                         runRawRealMode)
import           Pos.Ssc.Class          (SscConstraint, SscParams)
import           Pos.Statistics         (NoStatsT (..))
import           Pos.Util.UserSecret    (UserSecret, peekUserSecret)

import           Pos.Wallet.KeyStorage  (runKeyStorageRaw)
import           Pos.Wallet.WalletMode  (WalletRealMode)

-- | WalletMode runner.
runWalletMode
    :: forall ssc a.
       SscConstraint ssc
    => KademliaDHTInstance -> NodeParams -> SscParams ssc
    -> STM.TVar UserSecret -> WalletRealMode ssc a -> IO a
runWalletMode inst np sscnp tvar = runRawRealMode inst np sscnp listeners .
                                   flip runKeyStorageRaw tvar
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
    -> FilePath
    -> IO ()
runWallet inst plugins np sscnp keyfile = do
    tvar <- STM.newTVarIO =<< peekUserSecret keyfile
    runWalletMode inst np sscnp tvar $
        lift . getNoStatsT . runNode @ssc $
        map (NoStatsT . flip runKeyStorageRaw tvar) plugins
