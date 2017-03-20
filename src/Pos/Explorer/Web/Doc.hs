{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

-- | Documentation of cardano explorer web API.

module           Pos.Explorer.Web.Doc           (walletDocsText) where

import           Data.Time                      (defaultTimeLocale,
                                                 parseTimeOrError)
import           Data.Time.Clock.POSIX          (POSIXTime,
                                                 utcTimeToPOSIXSeconds)

import           Servant.API                    (Capture, QueryParam)
import           Servant.Docs                   (API, DocCapture (..),
                                                 DocIntro (..),
                                                 DocQueryParam (..),
                                                 ParamKind (Normal),
                                                 ToCapture (toCapture),
                                                 ToParam (toParam),
                                                 ToSample (toSamples),
                                                 docsWithIntros, markdown,
                                                 pretty)
import           Universum

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..),
                                                 CAddressSummary (..),
                                                 CBlockEntry (..),
                                                 CBlockSummary (..), CHash (..),
                                                 CSearchId (..), CTxEntry (..),
                                                 CTxId (..), CHashSearchResult (..),
                                                 CTxSummary (..))
import           Pos.Explorer.Web.Error         (ExplorerError (..))
import           Pos.Types                      (mkCoin)


walletDocs :: API
walletDocs = docsWithIntros intros (Servant.Docs.pretty explorerApi)
-- walletDocs = docsWith defaultDocOptions intros extras (pretty explorerApi)
-- walletDocs = docs (pretty explorerApi)

walletDocsText :: Text
walletDocsText = toText $ markdown walletDocs

intros :: [DocIntro]
intros =
    [ DocIntro
          "Documentation of cardano-explorer web API"
          ["This is very first version, don't expect it to be smart."]
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

instance ToCapture (Capture "hash" CSearchId) where
    toCapture Proxy =
        DocCapture
        { _capSymbol = "hash"
        , _capDesc = "Search id by which the user can find address, block or transaction"
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

-- sample data --
--------------------------------------------------------------------------------
posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

sampleAddressSummary :: CAddressSummary
sampleAddressSummary = CAddressSummary
    { caAddress = CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv"
    , caTxNum   = 0
    , caBalance = mkCoin 0
    , caTxList  = []
    }
--------------------------------------------------------------------------------

{-

data ExplorerError =
    -- | Some internal error.
    Internal !Text
    deriving (Show, Generic)

-}

instance ToSample ExplorerError where
    toSamples Proxy = [("Sample error", Internal "This is an example error")]

{-

data CHashSearchResult
    = AddressFound CAddressSummary
    | BlockFound CBlockSummary
    | FoundTransaction CTxSummary
    deriving (Show, Generic)

-}

instance ToSample CHashSearchResult where
    toSamples Proxy = [("Sample search result, address found", AddressFound sampleAddressSummary)]

{-

utcTimeToPOSIXSeconds ((reads "2011-11-19 18:28:r52.607875 UTC") :: UTCTime)

data CBlockEntry = CBlockEntry
    { cbeBlkHash    :: !CHash
    , cbeHeight     :: !Word
    , cbeTimeIssued :: !(Maybe POSIXTime)
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !Coin
    , cbeSize       :: !Word64
    , cbeRelayedBy  :: !(Maybe Text)

-}

instance ToSample CBlockEntry where
    toSamples Proxy = [("Sample block entry", sample)]
      where
        sample = CBlockEntry
            { cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
            , cbeHeight     = 10
            , cbeTimeIssued = Nothing
            , cbeTxNum      = 0
            , cbeTotalSent  = mkCoin 0
            , cbeSize       = 390
            , cbeRelayedBy  = Nothing
            }

{-

data CBlockSummary = CBlockSummary
    { cbsEntry      :: !CBlockEntry
    , cbsPrevHash   :: !CHash
    , cbsNextHash   :: !(Maybe CHash)
    , cbsMerkleRoot :: !CHash
    } deriving (Show, Generic)

-}

instance ToSample CBlockSummary where
    toSamples Proxy = [("Sample block summary", sample)]
      where
        sample = CBlockSummary
            { cbsEntry      = CBlockEntry
                                { cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
                                , cbeHeight     = 10
                                , cbeTimeIssued = Nothing
                                , cbeTxNum      = 0
                                , cbeTotalSent  = mkCoin 0
                                , cbeSize       = 390
                                , cbeRelayedBy  = Nothing
                                }
            , cbsPrevHash   = CHash "d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e"
            , cbsNextHash   = Just (CHash "d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1")
            , cbsMerkleRoot = CHash "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
            }

{-

data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !(Maybe POSIXTime)
    , cteAmount     :: !Coin
    } deriving (Show, Generic)

-}

instance ToSample CTxEntry where
    toSamples Proxy = [("Sample transaction entry", sample)]
      where
        sample = CTxEntry
            { cteId         = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
            , cteTimeIssued = posixTime
            , cteAmount     = mkCoin 33333
            }

{-

data CTxSummary = CTxSummary
    { ctsId              :: !CTxId
    , ctsTxTimeIssued    :: !(Maybe POSIXTime)
    , ctsBlockTimeIssued :: !(Maybe POSIXTime)
    , ctsBlockHeight     :: !(Maybe Word)
    , ctsRelayedBy       :: !(Maybe CNetworkAddress)
    , ctsTotalInput      :: !Coin
    , ctsTotalOutput     :: !Coin
    , ctsFees            :: !Coin
    , ctsInputs          :: ![(CAddress, Coin)]
    , ctsOutputs         :: ![(CAddress, Coin)]
    } deriving (Show, Generic)

-}

instance ToSample CTxSummary where
    toSamples Proxy = [("Sample transaction summary", sample)]
      where
        sample = CTxSummary
            { ctsId              = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
            , ctsTxTimeIssued    = posixTime
            , ctsBlockTimeIssued = Nothing
            , ctsBlockHeight     = Just 11
            , ctsRelayedBy       = Nothing
            , ctsTotalInput      = mkCoin 33333
            , ctsTotalOutput     = mkCoin 33333
            , ctsFees            = mkCoin 0
            , ctsInputs          = [(CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv", mkCoin 33333)]
            , ctsOutputs         = [(CAddress "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ", mkCoin 33333)]
            }



{-

data CAddressSummary = CAddressSummary
    { caAddress :: !CAddress
    , caTxNum   :: !Word
    , caBalance :: !Coin
    , caTxList  :: ![CTxBrief]
    } deriving (Show, Generic)

-}

instance ToSample CAddressSummary where
    toSamples Proxy = [("Sample address summary", sample)]
      where
        sample = sampleAddressSummary
