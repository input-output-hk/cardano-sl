module Main
       ( main
       ) where

import           Language.PureScript.Bridge         (BridgePart, buildBridge,
                                                     defaultBridge, mkSumType, typeName,
                                                     writePSTypes, (<|>), (^==))
import           Language.PureScript.Bridge.PSTypes (psInt)
import           Universum

import qualified Pos.Types                          as PT
import qualified Pos.Util.BackupPhrase              as BP
import qualified Pos.Wallet.Web                     as CT

import           PSTypes                            (psHash, psPosixTime)

main :: IO ()
main =
    writePSTypes
      "daedalus/src/Generated"
      (buildBridge customBridge)
      [ mkSumType (Proxy @CT.WalletError)
      , mkSumType (Proxy @CT.CCurrency)
      , mkSumType (Proxy @CT.CWalletMeta)
      , mkSumType (Proxy @CT.CWalletInit)
      , mkSumType (Proxy @CT.CWalletType)
      , mkSumType (Proxy @CT.CWallet)
      , mkSumType (Proxy @CT.CProfile)
      , mkSumType (Proxy @CT.CTType)
      , mkSumType (Proxy @CT.CTxMeta)
      , mkSumType (Proxy @CT.CTExMeta)
      , mkSumType (Proxy @CT.CAddress)
      , mkSumType (Proxy @CT.CHash)
      , mkSumType (Proxy @CT.CTxId)
      , mkSumType (Proxy @CT.CTx)
      , mkSumType (Proxy @CT.NotifyEvent)
      , mkSumType (Proxy @CT.SyncProgress)
      , mkSumType (Proxy @CT.CUpdateInfo)
      , mkSumType (Proxy @CT.CWalletRedeem)
      , mkSumType (Proxy @CT.CPaperVendWalletRedeem)
      , mkSumType (Proxy @CT.CInitialized)
      , mkSumType (Proxy @CT.CPassPhrase)
      , mkSumType (Proxy @CT.CWalletAssurance)
      , mkSumType (Proxy @CT.CCoin)
      , mkSumType (Proxy @PT.Coin)
      , mkSumType (Proxy @PT.ChainDifficulty)
      , mkSumType (Proxy @PT.BlockVersion)
      , mkSumType (Proxy @PT.SoftwareVersion)
      , mkSumType (Proxy @PT.ApplicationName)
      , mkSumType (Proxy @BP.BackupPhrase)
      ]
  where
      customBridge =
          defaultBridge <|> posixTimeBridge <|> wordBridge <|>
          word8Bridge <|> word16Bridge <|> word32Bridge <|>
          word64Bridge <|> hashBridge

posixTimeBridge :: BridgePart
posixTimeBridge = typeName ^== "NominalDiffTime" >> pure psPosixTime

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> pure psInt

word8Bridge :: BridgePart
word8Bridge = typeName ^== "Word8" >> pure psInt

word16Bridge :: BridgePart
word16Bridge = typeName ^== "Word16" >> pure psInt

word32Bridge :: BridgePart
word32Bridge = typeName ^== "Word32" >> pure psInt

word64Bridge :: BridgePart
word64Bridge = typeName ^== "Word64" >> pure psInt

hashBridge :: BridgePart
hashBridge = typeName ^== "Hash" >> pure psHash
