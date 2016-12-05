{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

#ifdef WITH_WALLET
import           Data.List          ((!!))

import           Pos.Crypto         (unsafeHash)
import           Pos.DHT            (DHTNodeType (..))
import           Pos.Genesis        (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher       (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Ssc.GodTossing (GtParams (..), SscGodTossing, genesisVssKeyPairs)
#ifdef WITH_WEB
import           Pos.Wallet.Web     (walletServeWeb)
#endif

import           WalletOptions      (WalletCommand (..), WalletOptions (..),
                                     getWalletOptions)

main :: IO ()
main = do
    WalletOptions {..} <- getWalletOptions
    case woCommand of
        SubmitTx {..} -> panic "I am broken and won't work today"
#ifdef WITH_WEB
        ServeWallet {..} -> do
            walletServeWeb swPort
#else
            panic "Web is disabled!"
#endif
#else
main :: IO ()
main = panic "Wallet is disabled!"
#endif
