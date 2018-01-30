-- | Functions for random values.

module Bench.Pos.Wallet.Random
    ( pickRandomWalletFrom
    , pickRandomAccountIn
    , pickRandomAddressIn
    , waitRandom
    ) where

import           Universum

import           Control.Concurrent         (threadDelay)
import qualified Data.List.NonEmpty         as NE
import           Data.List.NonEmpty         ((!!))
import           System.Random              (randomRIO)

import           Bench.Pos.Wallet.Types     (Wallet (..), WalletAccount (..), WalletsConfig (..))
import           Pos.Wallet.Web.ClientTypes (Addr, CId (..))

pickRandomWalletFrom
    :: MonadIO m
    => WalletsConfig
    -> m (Wallet)
pickRandomWalletFrom WalletsConfig {..} = pickRandomElementFrom wallets

pickRandomAccountIn
    :: MonadIO m
    => Wallet
    -> m WalletAccount
pickRandomAccountIn (Wallet _ allAccounts) = pickRandomElementFrom allAccounts

pickRandomAddressIn
    :: MonadIO m
    => WalletAccount
    -> m (CId Addr)
pickRandomAddressIn (WalletAccount _ allAddresses) = pickRandomElementFrom allAddresses

pickRandomElementFrom :: MonadIO m => NonEmpty a -> m a
pickRandomElementFrom aList = do
    someIndex <- liftIO $ randomRIO (0, NE.length aList - 1)
    return $ aList !! someIndex

-- | Waiting some random delay.
-- Values of @from@ and @to@ are already checked:
-- both are positive and @to@ is greater than @from@.
waitRandom :: (Double, Double) -> IO ()
waitRandom (from, to) =
    if thereIsNoDelay
        then return ()
        else randomRIO (fromInMicrosec, toInMicrosec) >>= threadDelay
  where
    fromInMicrosec, toInMicrosec :: Int
    fromInMicrosec = truncate $ from * asMicrosec
    toInMicrosec   = truncate $ to * asMicrosec
    thereIsNoDelay = fromInMicrosec == 0 && toInMicrosec == 0
    asMicrosec = 1000000
