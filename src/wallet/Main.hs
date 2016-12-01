{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import           Data.List          ((!!))

import           Pos.Crypto         (unsafeHash)
import           Pos.DHT            (DHTNodeType (..))
import           Pos.Genesis        (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher       (BaseParams (..), LoggingParams (..), NodeParams (..),
                                     submitTxReal)
import           Pos.Ssc.GodTossing (SscGodTossing, genesisVssKeyPairs)
import           Pos.Wallet.Web     (walletServeWeb)

import           WalletOptions      (WalletCommand (..), WalletOptions (..),
                                     getWalletOptions)

main :: IO ()
main = do
    WalletOptions {..} <- getWalletOptions
    case woCommand of
        SubmitTx {..} -> do
            let i = fromIntegral stGenesisIdx
            let params =
                    NodeParams
                    { npDbPath = Nothing
                    , npRebuildDb = False
                    , npSystemStart = 1477706355381569 --arbitrary value
                    , npSecretKey = genesisSecretKeys !! i
                    , npVssKeyPair = genesisVssKeyPairs !! i
                    , npBaseParams = BaseParams
                                      { bpLoggingParams = LoggingParams
                                                          { lpRunnerTag     = "wallet"
                                                          , lpHandlerPrefix = stLogsPrefix
                                                          , lpConfigPath    = stLogConfig
                                                          }
                                      , bpPort = 24962
                                      , bpDHTPeers = stDHTPeers
                                      , bpDHTKeyOrType = Right DHTClient
                                      , bpDHTExplicitInitial = False
                                      }
                    , npCustomUtxo = Nothing
                    , npTimeLord = False
                    , npJLFile = Nothing
                    , npSscEnabled = False
                    }
            let addr = genesisAddresses !! i
            let txId = unsafeHash addr
            submitTxReal @SscGodTossing params (txId, 0) (addr, 10)
        ServeWallet {..} -> do
            walletServeWeb swPort
