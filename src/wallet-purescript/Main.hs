module Main
       ( main
       ) where

import           Language.PureScript.Bridge                (BridgePart, buildBridge,
                                                            defaultBridge, mkSumType,
                                                            typeName, writePSTypes, (<|>),
                                                            (^==))
import           Language.PureScript.Bridge.PSTypes        (psInt)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Universum

import qualified Pos.Types                                 as PT
import qualified Pos.Util.BackupPhrase                     as BP
import qualified Pos.Wallet.Web                            as CT

import           PSTypes                                   (psInt53, psPosixTime)


main :: IO ()
main =
    writePSTypes
      "daedalus/src/Generated"
      (buildBridge customBridge)
      [ mkSumType (Proxy @CT.WalletError)
      , mkSumType (Proxy @CT.CWalletMeta)
      , mkSumType (Proxy @CT.CWalletInit)
      , mkSumType (Proxy @CT.CWallet)
      , mkSumType (Proxy @CT.CWalletSet)
      , mkSumType (Proxy @CT.CWalletSetInit)
      , mkSumType (Proxy @CT.CProfile)
      , mkSumType (Proxy @CT.CTxMeta)
      , mkSumType (Proxy @CT.CTExMeta)
      , mkSumType (Proxy @CT.CWalletAddress)
      , mkSumType (Proxy @CT.CWalletSetMeta)
      , mkSumType (Proxy @CT.CAccount)
      , mkSumType (Proxy @CT.CAccountAddress)
      , mkSumType (Proxy @CT.WS)
      , mkSumType (Proxy @CT.Acc)
      , mkSumType (Proxy @(CT.CAddress A))
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
      , mkSumType (Proxy @CT.CWalletSetAssurance)
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
          word64Bridge

posixTimeBridge :: BridgePart
posixTimeBridge = typeName ^== "NominalDiffTime" >> pure psPosixTime

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> pure psInt53

word8Bridge :: BridgePart
word8Bridge = typeName ^== "Word8" >> pure psInt

word16Bridge :: BridgePart
word16Bridge = typeName ^== "Word16" >> pure psInt

word32Bridge :: BridgePart
word32Bridge = typeName ^== "Word32" >> pure psInt53

-- FIXME: this is not actually correct, but we don't
-- use >= Int53 except for Coin, and Coin is representat
-- as String in `0.4` branch
word64Bridge :: BridgePart
word64Bridge = typeName ^== "Word64" >> pure psInt53
