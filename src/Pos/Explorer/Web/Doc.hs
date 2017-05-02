{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


-- | Documentation of cardano explorer web API.
module Pos.Explorer.Web.Doc
       ( walletDocsText
       , walletTableDocsText
       ) where

import           Control.Lens                   ((<>~))
import qualified Data.ByteString.Char8          as BSC
import qualified Data.HashMap.Strict            as HM
import           Data.String                    as DS
import           Data.Time                      (defaultTimeLocale, parseTimeOrError)
import           Data.Time.Clock.POSIX          (POSIXTime, utcTimeToPOSIXSeconds)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CBlockEntry (..), CBlockSummary (..),
                                                 CHash (..), CTxBrief (..), CTxEntry (..),
                                                 CTxId (..), CTxSummary (..), EpochIndex,
                                                 mkCCoin)
import           Pos.Explorer.Web.Error         (ExplorerError (..))
import           Pos.Types                      (mkCoin)
import           Servant.API                    (Capture, QueryParam)
import           Servant.Docs                   (API, Action, DocCapture (..),
                                                 DocIntro (..), DocNote (..),
                                                 DocQueryParam (..), Endpoint,
                                                 ExtraInfo (..), ParamKind (Normal),
                                                 ToCapture (toCapture), ToParam (toParam),
                                                 ToSample (toSamples), apiEndpoints,
                                                 apiIntros, capDesc, capSymbol, captures,
                                                 defAction, defEndpoint,
                                                 defaultDocOptions, docsWith, introBody,
                                                 introTitle, markdown, method, noteBody,
                                                 notes, paramDesc, paramName, params,
                                                 path, pretty)
import           Universum



walletDocs :: API
walletDocs = docsWith defaultDocOptions intros extras (Servant.Docs.pretty explorerApi)
-- walletDocs = docsWithIntros intros (Servant.Docs.pretty explorerApi)
-- walletDocs = docs (pretty explorerApi)

walletDocsText :: Text
walletDocsText = toText $ markdown walletDocs

walletTableDocsText :: Text
walletTableDocsText = toText $ markdownTable walletDocs

intros :: [DocIntro]
intros = [DocIntro "Explorer Backend API"
    [ "Currently, the explorer's API provides a series of methods to work with `cardano-sl`. The `servant` Haskell library that provides a modular approach to API-building was used. This library uses combinators to both build atomic HTTP actions and to glue these atomic methods together to form larger and more complete APIs."
    , "If the event requests fail, there is a `ExplorerError` type, which is simply a wrapper over `Text` to show what happened."
    , "Currently, the explorer's API supports the following operations (see Comments below):"]]

extras :: ExtraInfo api
extras =
    ExtraInfo . HM.fromList $
    [ (defEndpoint  & path <>~ ["api", "blocks", "last"], defAction & notes <>~ [ DocNote "Description" ["Get last block."] ])
    , (defEndpoint  & path <>~ ["api", "blocks", "summary", ":hash"], defAction & notes <>~ [ DocNote "Description" ["Get block summary."] ])
    , (defEndpoint  & path <>~ ["api", "blocks", "txs", ":hash"], defAction & notes <>~ [ DocNote "Description" ["Get block transactions."] ])
    , (defEndpoint  & path <>~ ["api", "txs", "last"], defAction & notes <>~ [ DocNote "Description" ["Get last transaction."] ])
    , (defEndpoint  & path <>~ ["api", "txs", "summary", ":txid"], defAction & notes <>~ [ DocNote "Description" ["Get transaction summary."] ])
    , (defEndpoint  & path <>~ ["api", "addresses", "summary", ":address"], defAction & notes <>~ [ DocNote "Description" ["Get address summary."] ])
    , (defEndpoint  & path <>~ ["api", "search", ":hash"], defAction & notes <>~ [ DocNote "Description" ["Search for transaction, block or address."] ])
    ]

instance ToParam (QueryParam "offset" Word) where
    toParam Proxy =
        DocQueryParam
        { _paramName    = "offset"
        , _paramValues  = ["0", "100"]
        , _paramDesc    = "Offset this many transactions"
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

instance ToCapture (Capture "hash" CHash) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "hash"
        , _capDesc = "Hash"
        }

instance ToCapture (Capture "txid" CTxId) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "txid"
        , _capDesc = "Transaction id"
        }

instance ToCapture (Capture "address" CAddress) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "address"
        , _capDesc = "Address"
        }

instance ToCapture (Capture "epoch" EpochIndex) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "epoch"
        , _capDesc = "Epoch index"
        }

instance ToParam (QueryParam "slot" Word16) where
    toParam Proxy =
        DocQueryParam
        { _paramName    = "slot"
        , _paramValues  = ["0", "1", "2"]
        , _paramDesc    = "Slot index"
        , _paramKind    = Normal
        }

-- sample data --
--------------------------------------------------------------------------------
posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

sampleAddressSummary :: CAddressSummary
sampleAddressSummary = CAddressSummary
    { caAddress = CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv"
    , caTxNum   = 0
    , caBalance = mkCCoin $ mkCoin 0
    , caTxList  = []
    }
--------------------------------------------------------------------------------

instance ToSample ExplorerError where
    toSamples Proxy = [("Sample error", Internal "This is an example error")]

instance ToSample CBlockEntry where
    toSamples Proxy = [("Sample block entry", sample)]
      where
        sample = CBlockEntry
            { cbeEpoch      = 37294
            , cbeSlot       = 10
            , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
            , cbeTimeIssued = Nothing
            , cbeTxNum      = 0
            , cbeTotalSent  = mkCCoin $ mkCoin 0
            , cbeSize       = 390
            , cbeRelayedBy  = Nothing
            }

instance ToSample Int where
    toSamples Proxy = [("Sample Int", 1)]

instance ToSample CBlockSummary where
    toSamples Proxy = [("Sample block summary", sample)]
      where
        sample = CBlockSummary
            { cbsEntry      = CBlockEntry
                                { cbeEpoch      = 37294
                                , cbeSlot       = 10
                                , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
                                , cbeTimeIssued = Nothing
                                , cbeTxNum      = 0
                                , cbeTotalSent  = mkCCoin $ mkCoin 0
                                , cbeSize       = 390
                                , cbeRelayedBy  = Nothing
                                }
            , cbsPrevHash   = CHash "d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e"
            , cbsNextHash   = Just (CHash "d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1")
            , cbsMerkleRoot = CHash "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
            }

instance ToSample CTxEntry where
    toSamples Proxy = [("Sample transaction entry", sample)]
      where
        sample = CTxEntry
            { cteId         = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
            , cteTimeIssued = posixTime
            , cteAmount     = mkCCoin $ mkCoin 33333
            }

instance ToSample CTxBrief where
    toSamples Proxy = [("Sample transaction brief description", sample)]
      where
        sample = CTxBrief
            { ctbId         = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
            , ctbTimeIssued = posixTime
            , ctbInputs     = [(CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv", mkCCoin $ mkCoin 33333)]
            , ctbOutputs    = [(CAddress "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ", mkCCoin $ mkCoin 33333)]
            , ctbInputSum   = mkCCoin $ mkCoin 33333
            , ctbOutputSum  = mkCCoin $ mkCoin 33333
            }

instance ToSample CTxSummary where
    toSamples Proxy = [("Sample transaction summary", sample)]
      where
        sample = CTxSummary
            { ctsId              = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
            , ctsTxTimeIssued    = posixTime
            , ctsBlockTimeIssued = Nothing
            , ctsBlockHeight     = Just 11
            , ctsRelayedBy       = Nothing
            , ctsTotalInput      = mkCCoin $ mkCoin 33333
            , ctsTotalOutput     = mkCCoin $ mkCoin 33333
            , ctsFees            = mkCCoin $ mkCoin 0
            , ctsInputs          = [(CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv", mkCCoin $ mkCoin 33333)]
            , ctsOutputs         = [(CAddress "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ", mkCCoin $ mkCoin 33333)]
            }

instance ToSample CAddressSummary where
    toSamples Proxy = [("Sample address summary", sample)]
      where
        sample = sampleAddressSummary

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
