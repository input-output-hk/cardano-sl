{-# LANGUAGE DataKinds #-}

-- | Documentation of wallet web API.

module Pos.Wallet.Web.Doc
       ( walletDocsText
       , walletTableDocsText
       ) where

import           Control.Lens               ((<>~))
import qualified Data.ByteString.Char8      as BSC
import qualified Data.HashMap.Strict        as HM
import           Data.String                as DS
import           Data.Time                  (defaultTimeLocale, parseTimeOrError)
import           Data.Time.Clock.POSIX      (POSIXTime, utcTimeToPOSIXSeconds)
import           Network.HTTP.Types.Method  (methodDelete, methodGet, methodPost,
                                             methodPut)
import           Servant.API                (Capture, QueryParam)
import           Servant.Docs               (API, Action, DocCapture (..), DocIntro (..),
                                             DocNote (..), DocQueryParam (..), Endpoint,
                                             ExtraInfo (..), ParamKind (Normal),
                                             ToCapture (toCapture), ToParam (toParam),
                                             ToSample (toSamples), apiEndpoints,
                                             apiIntros, capDesc, capSymbol, captures,
                                             defAction, defEndpoint, defaultDocOptions,
                                             docsWith, introBody, introTitle, markdown,
                                             method, noteBody, notes, paramDesc,
                                             paramName, params, path, pretty,
                                             singleSample)
import           System.IO.Unsafe           (unsafePerformIO)
import           Universum

import           Data.Default               (Default (def))
import           Pos.Aeson.ClientTypes      ()
import           Pos.Constants              (curSoftwareVersion)
import           Pos.Crypto                 (keyGen)
import           Pos.Types                  (BlockVersion (..), Coin, SoftwareVersion,
                                             makePubKeyAddress, mkCoin)
import           Pos.Util.BackupPhrase      (BackupPhrase, mkBackupPhrase12)
import           Pos.Wallet.Web.Api         (walletApi)
import           Pos.Wallet.Web.ClientTypes (CAddress (..), CCurrency (..), CHash (..),
                                             CInitialized (..), CPassPhrase,
                                             CPostVendWalletRedeem (..), CProfile (..),
                                             CTType (..), CTx (..), CTxId, CTxMeta (..),
                                             CUpdateInfo (..), CWallet (..),
                                             CWalletAssurance (..), CWalletInit (..),
                                             CWalletMeta (..), CWalletRedeem (..),
                                             CWalletType (..), SyncProgress,
                                             addressToCAddress, mkCTxId)
import           Pos.Wallet.Web.Error       (WalletError (..))



walletDocs :: API
walletDocs = docsWith defaultDocOptions intros extras (Servant.Docs.pretty walletApi)

walletDocsText :: Text
walletDocsText = toText $ markdown walletDocs

walletTableDocsText :: Text
walletTableDocsText = toText $ markdownTable walletDocs

intros :: [DocIntro]
intros = [DocIntro "Wallet Backend API"
    [ "Currently, the wallet's API provides a series of methods to work with wallets. \
      \The `servant` Haskell library that provides a modular approach to API-building was used. \
      \This library uses combinators to both build atomic HTTP actions and to glue these atomic \
      \methods together to form larger and more complete APIs."
    , "If the event requests fail, there is a `WalletError` type, \
      \which is simply a wrapper over `Text` to show what happened."
    , "Please note that:"
    , "* The code `Post '[JSON]` and `Get '[JSON]` indicates that the type of the contents \
      \in the message will be `[JSON]`. \n* `ReqBody '[JSON] t` extracts the request \
      \body `[JSON]` as a value of type `t`."
    , "Currently, the wallet's API supports the following operations (see Comments below):"]
    ]

-- [CSL-234]: this is unsafe solution, but I didn't manage to make
-- safe one work :(
extras :: ExtraInfo api
extras =
    ExtraInfo . HM.fromList $
    [ (defEndpoint  & path <>~ ["api", "wallets"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch all wallets to which the system has access to."] ])
    , (defEndpoint  & path <>~ ["api", "wallets"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Create a new wallet."] ])
    , (defEndpoint  & path <>~ ["api", "wallets", ":walletId"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch the wallet related to a given address address, if it exists."] ])
    , (defEndpoint  & path <>~ ["api", "wallets", ":walletId"] & method .~ methodDelete
    , defAction & notes <>~ [ DocNote "Description" ["Delete the wallet associated to an address."] ])
    , (defEndpoint  & path <>~ ["api", "wallets", ":walletId"] & method .~ methodPut
    , defAction & notes <>~ [ DocNote "Description" ["Given an address and wallet meta-information, update the address’ wallet."] ])
    , (defEndpoint  & path <>~ ["api", "wallets", "keys"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Import wallet from a key."] ])
    , (defEndpoint  & path <>~ ["api", "wallets", "restore"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Recover the wallet associated to the given backup information `[3]`, if it exists."] ])
    , (defEndpoint  & path <>~ ["api", "addresses"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Returns all addresses contained in wallet."] ])
    , (defEndpoint  & path <>~ ["api", "addresses", ":address", "currencies", ":currency"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Reply with `True` if the address is valid, and `False` otherwise. `[4]`"] ])
    , (defEndpoint  & path <>~ ["api", "profile"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch the client’s current user profile - the datatype CProfile. [5]"] ])
    , (defEndpoint  & path <>~ ["api", "profile"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Update the user’s profile, returning the new one in the process."] ])
    , (defEndpoint  & path <>~ ["api", "update"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch information related to the next update."] ])
    , (defEndpoint  & path <>~ ["api", "update"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Apply the system’s most recent update."] ])
    , (defEndpoint  & path <>~ ["api", "settings", "slots", "duration"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch the value of current slot duration."] ])
    , (defEndpoint  & path <>~ ["api", "settings", "sync", "progress"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Synchronization progress."] ])
    , (defEndpoint  & path <>~ ["api", "settings", "version"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch the system’s version."] ])
    , (defEndpoint  & path <>~ ["api", "redemptions", "ada"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Redeem ADA from a token `[6]`, create and return a wallet with the redeemded ADA."] ])
    , (defEndpoint  & path <>~ ["api", "test", "reset"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["The key reset when running `dev` mode."] ])
    , (defEndpoint  & path <>~ ["api", "txs", "histories", ":address"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch a tuple with the list of transactions where the address took part in the \
      \index interval [skip + 1, limit], and its length. `[2]`"] ])
    , (defEndpoint  & path <>~ ["api", "txs", "histories", ":address", ":search"] & method .~ methodGet
    , defAction & notes <>~ [ DocNote "Description" ["Fetch a tuple with the list of transactions whose title has search as an infix, \
      \in the index interval [skip + 1, limit], and its length. `[2]`"] ])
    , (defEndpoint  & path <>~ ["api", "txs", "payments", ":address", ":transaction"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Add the transaction which has the given ID to the wallet’s transaction history, \
      \if such a transaction exists."] ])
    , (defEndpoint  & path <>~ ["api", "txs", "payments", ":from", ":to", ":amount"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Send coins in the default currency (presently, `ADA`) from an origin address \
      \to a destination address, without any transaction message or description. `[1]`"] ])
    , (defEndpoint  & path <>~ ["api", "txs", "payments", ":from", ":to", ":amount", ":currency", ":title", ":description"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Send coins with currency \
      \(presently, `ADA`) from an origin address to a destination address, with title and description."] ])
    , (defEndpoint  & path <>~ ["api", "reporting", "initialized"] & method .~ methodPost
    , defAction & notes <>~ [ DocNote "Description" ["Initialize reporting."] ])
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

instance ToCapture (Capture "passphrase" CPassPhrase) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "passphrase"
        , _capDesc = "Passphrase to wallet"
        }

-- sample data --
--------------------------------------------------------------------------------
posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

cWalletMeta :: CWalletMeta
cWalletMeta = CWalletMeta
                { cwType      = CWTPersonal
                , cwCurrency  = ADA
                , cwName      = "Personal Wallet"
                , cwAssurance = CWANormal
                , cwUnit      = 0
                }

ctxMeta :: CTxMeta
ctxMeta = CTxMeta
      { ctmCurrency    = ADA
      , ctmTitle       = "Transaction"
      , ctmDescription = "Transaction from A to B"
      , ctmDate        = posixTime
      }

backupPhrase :: BackupPhrase
backupPhrase = mkBackupPhrase12 [ "transfer"
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

instance ToSample CPostVendWalletRedeem where
    toSamples Proxy = singleSample sample
      where
        sample = CPostVendWalletRedeem
            { pvWalletId         = CAddress $ CHash "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ"
            , pvSeed             = "1354644684681"
            , pvBackupPhrase     = mkBackupPhrase12 ["garlic", "swim", "arrow", "globe", "note", "gossip", "cabin", "wheel", "sibling", "cigar", "person", "clap"]
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
            , cwMeta    = cWalletMeta
            }


instance ToSample CWalletMeta where
    toSamples Proxy = singleSample sample
      where
        sample = cWalletMeta

instance ToSample CWalletInit where
    toSamples Proxy = singleSample sample
      where
        sample = CWalletInit
            { cwBackupPhrase = backupPhrase
            , cwInitMeta     = cWalletMeta
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
        sample =
            CProfile
            { cpLocale      = ""
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

-- | Generate documentation in Markdown table format for the given 'API'.
markdownTable :: API -> String
markdownTable api = DS.unlines $
    introsStr (api ^. apiIntros)
    ++ ["| API | Endpoint | Parameter | Optional parameters | Description |"]
    ++ ["|-----|----------|-----------|---------------------|-------------|"]
    ++ (concatMap (uncurry printEndpoint) . sort . HM.toList $ api ^. apiEndpoints)

  where showPath :: [String] -> String
        showPath [] = "/"
        showPath ps = concatMap ('/' :) ps

        printEndpoint :: Endpoint -> Action -> [String]
        printEndpoint endpoint action =
            ["| " ++ str ++
            " | " ++ capturesStr (action ^. captures) ++
            " | " ++ paramsStr (action ^. params) ++
            " | " ++ notesStr (action ^. notes) ++
            " | "]
          where
            str = BSC.unpack (endpoint^.method) ++ " |" ++ " " ++ showPath (endpoint ^. path)

        introsStr :: [DocIntro] -> [String]
        introsStr = concatMap introStr
          where
            introStr :: DocIntro -> [String]
            introStr i =
                ("## " ++ i ^. introTitle) :
                "" :
                intersperse "" (i ^. introBody) ++
                [""]

        capturesStr :: [DocCapture] -> String
        capturesStr [] = []
        capturesStr l = concatMap captureStr l
          where
            captureStr cap = "`" ++ (cap ^. capSymbol) ++ "` - " ++ (cap ^. capDesc) ++ "<br/> "

        paramsStr :: [DocQueryParam] -> String
        paramsStr [] = []
        paramsStr l = concatMap paramStr l
          where
            paramStr param = "`" ++ param ^. paramName ++ "` - " ++ param ^. paramDesc ++ "<br/> "

        notesStr :: [DocNote] -> String
        notesStr = concatMap noteStr
          where
            noteStr :: DocNote -> String
            noteStr nt = DS.unwords (nt ^. noteBody) ++ "<br/> "
            -- noteStr nt = nt ^. noteTitle ++ " - " ++ DS.unwords (nt ^. noteBody) ++ "<br/> "
