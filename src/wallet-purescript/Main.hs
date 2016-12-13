module Main
       ( main
       ) where

import           Data.Proxy                         (Proxy (..))
import           Language.PureScript.Bridge         (BridgePart, buildBridge,
                                                     defaultBridge, mkSumType,
                                                     typeName, writePSTypes,
                                                     (<|>), (^==))
import           Language.PureScript.Bridge.PSTypes (psInt)
import           Universum

import qualified Pos.Types.Address                  as PT
import qualified Pos.Types.Types                    as PT
import qualified Pos.Wallet.Web.ClientTypes         as CT

import           PSTypes                            (psHash, psPosixTime)

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
      , mkSumType (Proxy :: Proxy PT.Coin)
      , mkSumType (Proxy :: Proxy PT.Tx)
      , mkSumType (Proxy :: Proxy PT.TxIn)
      , mkSumType (Proxy :: Proxy PT.TxOut)
      ]
  where
      customBridge =
          defaultBridge <|> posixTimeBridge <|> word8Bridge <|> word32Bridge <|>
          word64Bridge <|> hashBridge

posixTimeBridge :: BridgePart
posixTimeBridge = typeName ^== "NominalDiffTime" >> pure psPosixTime

word8Bridge :: BridgePart
word8Bridge = typeName ^== "Word8" >> pure psInt

word32Bridge :: BridgePart
word32Bridge = typeName ^== "Word32" >> pure psInt

word64Bridge :: BridgePart
word64Bridge = typeName ^== "Word64" >> pure psInt

hashBridge :: BridgePart
hashBridge = typeName ^== "Hash" >> pure psHash
