module Main
       ( main
       ) where

import           Data.Version                              (showVersion)
import           Language.PureScript.Bridge                (BridgePart, buildBridge,
                                                            defaultBridge, mkSumType,
                                                            typeName, writePSTypes, (<|>),
                                                            (^==))
import           Language.PureScript.Bridge.PSTypes        (psInt)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Options.Applicative.Simple                (execParser, fullDesc, header,
                                                            help, helper, info,
                                                            infoOption, long, progDesc)
import           Universum

import           Paths_cardano_sl                          (version)
import qualified Pos.Types                                 as PT
import qualified Pos.Util.BackupPhrase                     as BP
import qualified Pos.Wallet.Web                            as CT

import           PSTypes                                   (psInt53, psPosixTime)

showProgramInfoIfRequired :: IO ()
showProgramInfoIfRequired = void $ execParser programInfo
  where
    programInfo = info (helper <*> versionOption) $
        fullDesc <> progDesc ("Generate PureScript types based on Haskell types. "
                              <> "Program produces .purs-files in 'daedalus/src/Generated' subdirectory. "
                              <> "These types are used to build Daedalus wallet.")
                 <> header "Cardano SL PureScript types generator."

    versionOption = infoOption
        ("cardano-wallet-hs2purs-" <> showVersion version)
        (long "version" <> help "Show version.")

main :: IO ()
main = do
    showProgramInfoIfRequired
    writePSTypes
      "daedalus/src/Generated"
      (buildBridge customBridge)
      [ mkSumType (Proxy @CT.WalletError)
      , mkSumType (Proxy @CT.CAccountMeta)
      , mkSumType (Proxy @CT.CAccountInit)
      , mkSumType (Proxy @CT.CAccount)
      , mkSumType (Proxy @CT.CWallet)
      , mkSumType (Proxy @CT.CWalletInit)
      , mkSumType (Proxy @CT.CProfile)
      , mkSumType (Proxy @CT.CTxMeta)
      , mkSumType (Proxy @CT.CTExMeta)
      , mkSumType (Proxy @CT.CAccountId)
      , mkSumType (Proxy @CT.CWalletMeta)
      , mkSumType (Proxy @CT.CAddress)
      , mkSumType (Proxy @CT.CWAddressMeta)
      , mkSumType (Proxy @CT.Wal)
      , mkSumType (Proxy @CT.Addr)
      , mkSumType (Proxy @(CT.CId A))
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
