{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.WalletClient (
    withdraw
  , randomAmount
  ) where

import           Cardano.Wallet.API.V1.Types (Payment (..), V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, takeTMVar)
import           Control.Lens (re, to)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Crypto.Hash (Blake2b_256, Digest)
import qualified Crypto.Hash as CryptoHash
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text.Strict.Lens (utf8)
import           Pos.Core (Address (..), Coin (..))
import           Pos.Crypto.Signing (PassPhrase)
import           System.Random
import           System.Wlog (logError, logInfo, withSublogger)
import           Universum


import           Cardano.Faucet.Types

-- | Computes the amount of ADA (units in lovelace) to send in 'withdraw'
randomAmount :: (MonadIO m) => PaymentDistribution -> m Int
randomAmount (PaymentDistribution amt var)= do
    (f :: Float) <- liftIO $ randomRIO ((-1), 1)
    return $ round $ ((fromIntegral amt) + ((fromIntegral var) * f))

-- | Client function for the handler for the @/withdraw@ action
--
-- Simply sends a 'randomAmount' of ADA (units in lovelace )to the supplied
-- 'Address'
withdraw :: (MonadFaucet c m) => V1 Address -> m (Either WithdrawalQFull WithdrawalResult)
withdraw addr = withSublogger "WalletClient.withdraw" $ do
    paymentSource <- view (feSourceWallet . to cfgToPaymentSource)
    spendingPassword <- view (feSourceWallet . srcSpendingPassword)
    coin <- V1 . Coin . fromIntegral
              <$> (randomAmount =<< view (feFaucetConfig . fcPaymentDistribution))
    q <- view feWithdrawalQ
    let paymentDist = (V1.PaymentDistribution addr coin :| [])
        sp =  spendingPassword <&> view (re utf8 . to hashPwd . to V1)
        payment = Payment paymentSource paymentDist Nothing sp
    eRes <- liftIO $ sendToQueue q payment
    case eRes of
        Left e -> do
            logError "Queue is full"
            return $ Left e
        Right tvar -> do
            logInfo "Waiting for processing result"
            liftIO $ (Right <$> atomically (takeTMVar tvar))

-- | Sends the 'Payment' to the processor queue
--
-- Returns a 'TMVar' to wait on for the response from the node
-- See 'Cardano.Faucet.Init.processWithdrawals'
sendToQueue
    :: TBQ.TBQueue ProcessorPayload
    -> Payment
    -> IO (Either WithdrawalQFull (TMVar WithdrawalResult))
sendToQueue q payment = atomically $ do
        isFull <- TBQ.isFullTBQueue q
        if isFull
           then return $ Left WithdrawalQFull
           else do
            resTMVar <- newEmptyTMVar
            TBQ.writeTBQueue q  (ProcessorPayload payment resTMVar)
            return $ Right resTMVar

-- | Hashes bytestring password to the form expected by the wallet API
hashPwd :: ByteString -> PassPhrase
hashPwd  bs =
    let blake = CryptoHash.hash bs :: Digest (Blake2b_256)
    in BA.convert blake
