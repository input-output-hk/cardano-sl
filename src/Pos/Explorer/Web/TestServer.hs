{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Explorer.Web.TestServer (runMockServer) where

import           Data.Time                      (defaultTimeLocale, parseTimeOrError)
import           Data.Time.Clock.POSIX          (POSIXTime, utcTimeToPOSIXSeconds)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.Warp       (run)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api           (ExplorerApi, explorerApi)
import           Pos.Explorer.Web.ClientTypes   (CAddress (..), CAddressSummary (..),
                                                 CAddressType (..), CBlockEntry (..),
                                                 CBlockSummary (..), CHash (..),
                                                 CTxBrief (..), CTxEntry (..), CTxId (..),
                                                 CTxSummary (..), mkCCoin)
import           Pos.Explorer.Web.Error         (ExplorerError (..))
import           Pos.Types                      (EpochIndex, mkCoin)
import           Pos.Web                        ()
import           Servant.API                    ((:<|>) ((:<|>)))
import           Servant.Server                 (Handler, Server, serve)
import           Universum


----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

-- Run the server. Must be on the same port so we don't have to modify anything
runMockServer :: IO ()
runMockServer = run 8100 explorerApp

explorerApp :: Application
explorerApp = serve explorerApi explorerHandlers

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

explorerHandlers :: Server ExplorerApi
explorerHandlers =
      apiBlocksLast
    :<|>
      apiBlocksTotal
    :<|>
      apiBlocksPages
    :<|>
      apiBlocksPagesTotal
    :<|>
      apiBlocksSummary
    :<|>
      apiBlocksTxs
    :<|>
      apiTxsLast
    :<|>
      apiTxsSummary
    :<|>
      apiAddressSummary
    :<|>
      apiEpochSlotSearch
  where
    apiBlocksLast        = testBlocksLast
    apiBlocksTotal       = testBlocksTotal
    apiBlocksPages       = testBlocksPages
    apiBlocksPagesTotal  = testBlocksPagesTotal
    apiBlocksSummary     = testBlocksSummary
    apiBlocksTxs         = testBlocksTxs
    apiTxsLast           = testTxsLast
    apiTxsSummary        = testTxsSummary
    apiAddressSummary    = testAddressSummary
    apiEpochSlotSearch   = testEpochSlotSearch

--------------------------------------------------------------------------------
-- sample data --
--------------------------------------------------------------------------------
posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

sampleAddressSummary :: CAddressSummary
sampleAddressSummary = CAddressSummary
    { caAddress = CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv"
    , caType    = CPubKeyAddress
    , caTxNum   = 0
    , caBalance = mkCCoin $ mkCoin 0
    , caTxList  = []
    }
----------------------------------------------------------------
-- Test handlers
----------------------------------------------------------------

testBlocksLast
    :: Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError [CBlockEntry])
testBlocksLast _ _  = pure . pure $ [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Nothing
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin $ mkCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    }]

testBlocksPagesTotal
    :: Maybe Word
    -> Handler (Either ExplorerError Integer)
testBlocksPagesTotal _ = pure $ pure 10

testBlocksPages
    :: Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError (Integer, [CBlockEntry]))
testBlocksPages _ _  = pure . pure $ (1, [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Nothing
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin $ mkCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    }])

testBlocksSummary
    :: CHash
    -> Handler (Either ExplorerError CBlockSummary)
testBlocksSummary _ = pure . pure $ CBlockSummary
    { cbsEntry      = CBlockEntry
                        { cbeEpoch      = 37294
                        , cbeSlot       = 10
                        , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
                        , cbeTimeIssued = Nothing
                        , cbeTxNum      = 0
                        , cbeTotalSent  = mkCCoin $ mkCoin 0
                        , cbeSize       = 390
                        , cbeBlockLead  = Nothing
                        }
    , cbsPrevHash   = CHash "d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e"
    , cbsNextHash   = Just (CHash "d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1")
    , cbsMerkleRoot = CHash "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
    }

testBlocksTxs
    :: CHash
    -> Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError [CTxBrief])
testBlocksTxs _ _ _ = pure . pure $ [CTxBrief
    { ctbId         = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
    , ctbTimeIssued = posixTime
    , ctbInputs     = [(CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv", mkCCoin $ mkCoin 33333)]
    , ctbOutputs    = [(CAddress "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ", mkCCoin $ mkCoin 33333)]
    , ctbInputSum   = mkCCoin $ mkCoin 33333
    , ctbOutputSum  = mkCCoin $ mkCoin 33333
    }]

testBlocksTotal
    :: Handler (Either ExplorerError Integer)
testBlocksTotal = pure $ pure 333

testTxsLast
    :: Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError [CTxEntry])
testTxsLast _ _     = pure . pure $ [CTxEntry
    { cteId         = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"
    , cteTimeIssued = posixTime
    , cteAmount     = mkCCoin $ mkCoin 33333
    }]

testTxsSummary
    :: CTxId
    -> Handler (Either ExplorerError CTxSummary)
testTxsSummary _       = pure . pure $ CTxSummary
    { ctsId              = CTxId $ CHash "8aac4a6b18fafa2783071c66519332157ce96c67e88fc0cc3cb04ba0342d12a1"
    , ctsTxTimeIssued    = posixTime
    , ctsBlockTimeIssued = Nothing
    , ctsBlockHeight     = Just 13
    , ctsBlockEpoch      = Just 0
    , ctsBlockSlot       = Just 13
    , ctsBlockHash       = Just $ CHash "a9dea19829e80d9064cd0c33dccf5369638e43c62a090848342037e296120a35"
    , ctsRelayedBy       = Nothing
    , ctsTotalInput      = mkCCoin $ mkCoin 33333
    , ctsTotalOutput     = mkCCoin $ mkCoin 33333
    , ctsFees            = mkCCoin $ mkCoin 0
    , ctsInputs          = [(CAddress "19HxN7PseAPT93RftAh7bBmbnJU5gtH6QzvUyZXnbz9Y1UtYwPDdiCGkB2gwvC8CjBUtHXBij9j9Qb6JYgHPi6LtevDcFQ", mkCCoin $ mkCoin 97)]
    , ctsOutputs         =  [ (CAddress "19F6U1Go5B4KakVoCZfzCtqNAWhUBprxVzL3JsGu74TEwQnXPvAKPUbvG8o4Qe5RaY8Z7WKLfxmNFwBqPV1NQ2hRpKkdEN", mkCCoin $ mkCoin 94)
                            , (CAddress "1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8", mkCCoin $ mkCoin 3)
                            ]
    }

testAddressSummary
    :: CAddress
    -> Handler (Either ExplorerError CAddressSummary)
testAddressSummary _  = pure . pure $ sampleAddressSummary

testEpochSlotSearch
    :: EpochIndex
    -> Maybe Word16
    -> Handler (Either ExplorerError [CBlockEntry])
testEpochSlotSearch _ _ = pure . pure $ [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Nothing
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin $ mkCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    }]
