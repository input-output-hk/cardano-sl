module Main
       ( main
       ) where

import           Data.Proxy                 (Proxy (..))
import           Language.PureScript.Bridge (BridgePart, buildBridge,
                                             defaultBridge, mkSumType, typeName,
                                             writePSTypes, (<|>), (^==))
import           Universum

import qualified Pos.Types.Address          as PT
import qualified Pos.Wallet.Web.ClientTypes as CT

import           PSTypes                    (psPosixTime)

main :: IO ()
main =
    writePSTypes
      "src/wallet-purescript-bridge/src"
      (buildBridge customBridge)
      [ mkSumType (Proxy :: Proxy CT.CCurrency)
      , mkSumType (Proxy :: Proxy CT.CWalletMeta)
      , mkSumType (Proxy :: Proxy CT.CWalletType)
      , mkSumType (Proxy :: Proxy CT.CWallet)
      , mkSumType (Proxy :: Proxy CT.CProfile)
      , mkSumType (Proxy :: Proxy CT.CTType)
      , mkSumType (Proxy :: Proxy CT.CTxMeta)
      , mkSumType (Proxy :: Proxy CT.CTExMeta)
      , mkSumType (Proxy :: Proxy CT.CTx)
      , mkSumType (Proxy :: Proxy PT.Address)
      ]
  where
      customBridge =
          defaultBridge <|> posixTimeBridge

posixTimeBridge :: BridgePart
posixTimeBridge = typeName ^== "NominalDiffTime" >> pure psPosixTime
