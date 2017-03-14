{-# LANGUAGE DataKinds #-}

-- | Documentation of wallet web API.

module Pos.Wallet.Web.Doc
       ( walletDocsText
       ) where

import           Control.Lens               ((<>~))
import qualified Data.HashMap.Strict        as HM
import           Data.Time                  (defaultTimeLocale, parseTimeOrError)
import           Data.Time.Clock.POSIX      (POSIXTime, utcTimeToPOSIXSeconds)
import           Network.HTTP.Types.Method  (methodPost)
import           Servant.API                (Capture, QueryParam)
import           Servant.Docs               (API, DocCapture (..), DocIntro (..),
                                             DocNote (..), DocQueryParam (..),
                                             ExtraInfo (..), ParamKind (Normal),
                                             ToCapture (toCapture), ToParam (toParam),
                                             ToSample (toSamples), defAction, defEndpoint,
                                             defaultDocOptions, docsWith, markdown,
                                             method, notes, path, pretty, singleSample)
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum

import           Data.Default               (Default (def))
import           Pos.Aeson.ClientTypes      ()
import           Pos.Constants              (curSoftwareVersion)
import           Pos.Crypto                 (keyGen)
import           Pos.Types                  (BlockVersion (..), Coin, SoftwareVersion,
                                             makePubKeyAddress, mkCoin)
import           Pos.Util.BackupPhrase      (BackupPhrase, mkBackupPhrase)
import           Pos.Wallet.Web.Api         (walletApi)
import           Pos.Wallet.Web.ClientTypes (CAddress (..), CCurrency (..), CHash (..),
                                             CInitialized (..), CProfile (..),
                                             CTType (..), CTx (..), CTxId, CTxMeta (..),
                                             CUpdateInfo (..), CWallet (..),
                                             CWalletInit (..), CWalletMeta (..),
                                             CWalletRedeem (..), CWalletType (..),
                                             SyncProgress, addressToCAddress, mkCTxId)
import           Pos.Wallet.Web.Error       (WalletError (..))



walletDocs :: API
walletDocs = docsWith defaultDocOptions intros extras (Servant.Docs.pretty walletApi)

walletDocsText :: Text
walletDocsText = toText $ markdown walletDocs

intros :: [DocIntro]
intros =
    [ DocIntro
          "Documentation of cardano-wallet web API"
          ["This is very first version, don't expect it to be smart."]
    ]

-- [CSL-234]: this is unsafe solution, but I didn't manage to make
-- safe one work :(
extras :: ExtraInfo api
extras =
    ExtraInfo . HM.fromList $
    [ (defEndpoint & path <>~ ["addresses"], defAction & notes <>~ addressesNotes)
    , ( defEndpoint & path <>~ ["send"] & method .~ methodPost
      , defAction & notes <>~ sendNotes)
    ]
  where
    addressesNotes =
        [ DocNote
          { _noteTitle = "Description"
          , _noteBody = ["Returns all addresses contained in wallet."]
          }
        ]
    sendNotes =
        [ DocNote
          { _noteTitle = "Description"
          , _noteBody =
              ["Send coins from one address from wallet to arbitrary address"]
          }
        ]

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance ToCapture (Capture "walletId" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "walletId"
        , _capDesc = "WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses"
        }

instance ToCapture (Capture "from" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "from"
        , _capDesc = "Address from which coins should be sent."
        }

instance ToCapture (Capture "to" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "to"
        , _capDesc = "Destination address."
        }

instance ToCapture (Capture "amount" Coin) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "amount"
        , _capDesc = "Amount of coins to send."
        }

instance ToCapture (Capture "address" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "address"
        , _capDesc = "Address, history of which should be fetched"
        }

instance ToCapture (Capture "index" Word) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "index"
        , _capDesc = "Index of address to delete"
        }

instance ToCapture (Capture "transaction" CTxId) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "transaction"
        , _capDesc = "Transaction id"
        }

instance ToCapture (Capture "address" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "address"
        , _capDesc = "Address"
        }

instance ToCapture (Capture "description" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "description"
        , _capDesc = "Transaction description"
        }

instance ToCapture (Capture "title" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "title"
        , _capDesc = "Transaction title"
        }

instance ToCapture (Capture "search" Text) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "search"
        , _capDesc = "Wallet title search pattern"
        }

instance ToParam (QueryParam "skip" Word) where
    toParam Proxy =
        DocQueryParam
        { _paramName    = "skip"
        , _paramValues  = ["0", "100"]
        , _paramDesc    = "Skip this many transactions"
        , _paramKind    = Normal
        }

instance ToParam (QueryParam "limit" Word) where
    toParam Proxy =
        DocQueryParam
        { _paramName    = "limit"
        , _paramValues  = ["0", "100"]
        , _paramDesc    = "Max numbers of transactions to return"
        , _paramKind    = Normal
        }

instance ToCapture (Capture "currency" CCurrency) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "currency"
        , _capDesc = "Currency"
        }

instance ToCapture (Capture "time" POSIXTime) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "time"
        , _capDesc = "Postpone update until specific date/time"
        }

instance ToCapture (Capture "key" FilePath) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "key"
        , _capDesc = "File path to the secret key"
        }

-- sample data --
--------------------------------------------------------------------------------
posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

ctxMeta :: CTxMeta
ctxMeta = CTxMeta
      { ctmCurrency    = ADA
      , ctmTitle       = "Transaction"
      , ctmDescription = "Transaction from A to B"
      , ctmDate        = posixTime
      }

backupPhrase :: BackupPhrase
backupPhrase = mkBackupPhrase [ "transfer"
                              , "uniform"
                              , "grunt"
                              , "excess"
                              , "six"
                              , "veteran"
                              , "vintage"
                              , "warm"
                              , "confirm"
                              , "vote"
                              , "nephew"
                              , "allow"
                              ]
--------------------------------------------------------------------------------

instance ToSample WalletError where
    toSamples Proxy = singleSample (Internal "Sample error")

instance ToSample CWalletRedeem where
    toSamples Proxy = singleSample sample
      where
        sample = CWalletRedeem
            { crWalletId = CAddress $ CHash "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ"
            , crSeed     = "1354644684681"
            }

instance ToSample Coin where
    toSamples Proxy = singleSample (mkCoin 100500)

-- instance ToSample Address where
--     toSamples Proxy = singleSample $ genesisAddresses !! 0
--
-- FIXME!
instance ToSample CHash where
    toSamples Proxy = singleSample $ CHash "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv"

instance ToSample CWallet where
    toSamples Proxy = singleSample sample
      where
        sample = CWallet
            { cwAddress = CAddress $ CHash "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ"
            , cwAmount  = mkCoin 0
            , cwMeta    = CWalletMeta
                            { cwType     = CWTPersonal
                            , cwCurrency = ADA
                            , cwName     = "Personal Wallet"
                            }
            }


instance ToSample CWalletMeta where
    toSamples Proxy = singleSample sample
      where
        sample = CWalletMeta
            { cwType     = CWTPersonal
            , cwCurrency = ADA
            , cwName     = "Personal Wallet"
            }

instance ToSample CWalletInit where
    toSamples Proxy = singleSample sample
      where
        sample = CWalletInit
            { cwBackupPhrase = backupPhrase
            , cwInitMeta     = CWalletMeta
                                  { cwType     = CWTPersonal
                                  , cwCurrency = ADA
                                  , cwName     = "Personal Wallet"
                                  }
            }

instance ToSample CUpdateInfo where
    toSamples Proxy = singleSample sample
      where
        sample = CUpdateInfo
            { cuiSoftwareVersion = curSoftwareVersion
            , cuiBlockVesion     = BlockVersion
                                    { bvMajor = 25
                                    , bvMinor = 12
                                    , bvAlt   = 3
                                    }
            , cuiScriptVersion   = 15
            , cuiImplicit        = False
            , cuiVotesFor        = 2
            , cuiVotesAgainst    = 3
            , cuiPositiveStake   = mkCoin 10
            , cuiNegativeStake   = mkCoin 3
            }


instance ToSample CAddress where
    toSamples Proxy = singleSample . addressToCAddress . makePubKeyAddress . fst $
        unsafePerformIO keyGen

-- FIXME: this is required because of Wallet.Web.Api `type Cors...`
-- I don't really what should be sample for Cors ?
instance ToSample Text where
    toSamples Proxy = singleSample "Sample CORS"

instance ToSample () where
    toSamples Proxy = singleSample ()

instance ToSample CTx where
    toSamples Proxy = singleSample sample
      where
        sample = CTx
            { ctId            = mkCTxId "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ"
            , ctAmount        = mkCoin 0
            , ctConfirmations = 10
            , ctType          = CTOut ctxMeta
            }

instance ToSample CTxMeta where
    toSamples Proxy = singleSample sample
      where
        sample = ctxMeta

instance ToSample CProfile where
    toSamples Proxy = singleSample sample
      where
        sample = CProfile
            { cpName        = ""
            , cpEmail       = ""
            , cpPhoneNumber = ""
            , cpPwHash      = ""
            , cpPwCreated   = posixTime
            , cpLocale      = ""
            , cpPicture     = ""
            }

instance ToSample Word where
    toSamples Proxy = singleSample (101 :: Word)

instance ToSample BackupPhrase where
    toSamples Proxy = singleSample sample
      where
        sample = backupPhrase

instance ToSample SoftwareVersion where
    toSamples Proxy = singleSample curSoftwareVersion

instance ToSample SyncProgress where
    toSamples Proxy = singleSample def

instance ToSample CInitialized where
    toSamples Proxy = singleSample $ CInitialized 123 456


--
--instance ToSample Tx where
--    toSamples Proxy = singleSample $ Tx [TxIn hsh idx] [out]
--      where ((hsh, idx), (out, _)) = M.toList (genesisUtxo def) !! 0
