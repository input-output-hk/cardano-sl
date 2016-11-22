{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( submitTxReal
       ) where

import           Universum

import           Control.Concurrent.MVar     (newEmptyMVar, newMVar, takeMVar,
                                              tryReadMVar)
import           Control.Monad               (fail)
import           Control.Monad.Catch         (bracket)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.TimeWarp.Rpc        (BinaryP (..), Dialog, MonadDialog,
                                              NetworkAddress, Transfer, commLoggerName,
                                              runDialog, runTransfer)
import           Control.TimeWarp.Timed      (MonadTimed, currentTime, for, fork, fork_,
                                              killThread, repeatForever, runTimedIO, sec,
                                              sleepForever, wait)
import           Data.Default                (Default)
import           Data.List                   (nub)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           System.Directory            (doesDirectoryExist,
                                              removeDirectoryRecursive)
import           System.FilePath             ((</>))
import           System.Log.Logger           (removeAllHandlers)
import           System.Wlog                 (LoggerName (..), WithNamedLogger, logDebug,
                                              logError, logInfo, logWarning,
                                              traverseLoggerConfig, usingLoggerName)

import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (SysStartRequest (..), allListeners,
                                              noCacheMessageNames, sendTx, statsListeners,
                                              sysStartReqListener,
                                              sysStartReqListenerSlave,
                                              sysStartRespListener)
import           Pos.Constants               (RunningMode (..), defaultPeers,
                                              isDevelopment, runningMode)
import           Pos.Crypto                  (SecretKey, VssKeyPair, hash, sign)
import           Pos.DHT                     (DHTKey, DHTNode (dhtAddr), DHTNodeType (..),
                                              ListenerDHT, MonadDHT (..),
                                              filterByNodeType, mapListenerDHT,
                                              sendToNeighbors)
import           Pos.DHT.Real                (KademliaDHT, KademliaDHTConfig (..),
                                              KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Launcher.Runner         (runRealMode)
import           Pos.Launcher.Runner         (bracketDHTInstance)
import           Pos.Launcher.Scenario       (submitTx)
import           Pos.Ssc.Class               (SscConstraint)
import           Pos.State.Storage           (storageFromUtxo)
import           Pos.Statistics              (getNoStatsT, getStatsT)
import           Pos.Types                   (Address, Coin, TxId)
import           Pos.WorkMode                (RealMode)

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
