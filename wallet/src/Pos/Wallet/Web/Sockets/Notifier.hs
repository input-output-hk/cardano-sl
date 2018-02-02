{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

-- | Notifier logic

module Pos.Wallet.Web.Sockets.Notifier
       ( launchNotifier
       ) where

import           Universum

import           Control.Concurrent                (forkFinally)
import           Control.Lens                      ((.=))
import           Data.Default                      (Default (def))
import           Data.Time.Units                   (Microsecond, Second)
import           Serokell.Util                     (threadDelay)
import           Servant.Server                    (Handler, runHandler)
import           Servant.Utils.Enter               ((:~>) (..))
import           System.Wlog                       (logDebug)

import           Pos.Aeson.ClientTypes             ()
import           Pos.Aeson.WalletBackup            ()
import           Pos.DB                            (MonadGState (..))
import           Pos.Wallet.WalletMode             (connectedPeers, localChainDifficulty,
                                                    networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.ClientTypes        (spLocalCD, spNetworkCD, spPeers,
                                                    toCUpdateInfo)
import           Pos.Wallet.Web.Mode               (MonadWalletWebMode)
import           Pos.Wallet.Web.Sockets.Connection (notifyAll)
import           Pos.Wallet.Web.Sockets.Types      (NotifyEvent (..))
import           Pos.Wallet.Web.State              (askWalletDB, addUpdate)

-- FIXME: this is really inefficient. Temporary solution
launchNotifier :: MonadWalletWebMode m => (m :~> Handler) -> m ()
launchNotifier nat =
    void . liftIO $ mapM startForking
        [ dificultyNotifier
        , updateNotifier
        ]
  where
    cooldownPeriod :: Second
    cooldownPeriod = 5

    difficultyNotifyPeriod :: Microsecond
    difficultyNotifyPeriod = 500000  -- 0.5 sec

    -- networkResendPeriod = 10         -- in delay periods
    forkForever action = forkFinally action $ const $ do
        -- TODO: log error
        -- cooldown
        threadDelay cooldownPeriod
        void $ forkForever action
    -- TODO: use Servant.enter here
    -- FIXME: don't ignore errors, send error msg to the socket
    startForking = forkForever . void . runHandler . ($$) nat
    notifier period action = forever $ do
        liftIO $ threadDelay period
        action
    dificultyNotifier = void . flip runStateT def $ notifier difficultyNotifyPeriod $ do
        whenJustM networkChainDifficulty $
            \networkDifficulty -> do
                oldNetworkDifficulty <- use spNetworkCD
                when (Just networkDifficulty /= oldNetworkDifficulty) $ do
                    lift $ notifyAll $ NetworkDifficultyChanged networkDifficulty
                    spNetworkCD .= Just networkDifficulty

        localDifficulty <- localChainDifficulty
        oldLocalDifficulty <- use spLocalCD
        when (localDifficulty /= oldLocalDifficulty) $ do
            lift $ notifyAll $ LocalDifficultyChanged localDifficulty
            spLocalCD .= localDifficulty

        peers <- connectedPeers
        oldPeers <- use spPeers
        when (peers /= oldPeers) $ do
            lift $ notifyAll $ ConnectedPeersChanged peers
            spPeers .= peers

    updateNotifier = do
        db <- askWalletDB
        cps <- waitForUpdate
        bvd <- gsAdoptedBVData
        addUpdate db $ toCUpdateInfo bvd cps
        logDebug "Added update to wallet storage"
        notifyAll UpdateAvailable

    -- historyNotifier :: WalletWebMode m => m ()
    -- historyNotifier = do
    --     cAddresses <- myCIds
    --     for_ cAddresses $ \cAddress -> do
    --         -- TODO: is reading from acid RAM only (not reading from disk?)
    --         oldHistoryLength <- length . fromMaybe mempty <$> getAccountHistory cAddress
    --         newHistoryLength <- length <$> getHistory cAddress
    --         when (oldHistoryLength /= newHistoryLength) .
    --             notifyAll $ NewWalletTransaction cAddress
