module Main where

import Universum

import Cardano.Wallet.Client (WalletClient (..))

import Types
import Functions

-- | Here we want to run main when the (local) nodes
-- have started.
main :: IO ()
main = do
    -- Formatting
    putStrLn ("Starting the integration testing for wallet." :: Text)
    putStrLn ("The node should be running." :: Text)

    let actionDistribution = [ (CreateWallet, createProbability 50)
                             , (GetWallet, createProbability 50)
                             ]

    let walletClient :: forall m. WalletClient m
        walletClient = error "Missing"

    let walletState = WalletState mempty mempty 0

    -- some monadic fold or smth similar
    _ <- runActionCheck
        walletClient
        walletState
        actionDistribution

    pure ()

