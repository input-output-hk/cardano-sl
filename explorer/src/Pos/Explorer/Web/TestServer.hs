{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Explorer.Web.TestServer
       ( runMockServer
       ) where

import           Universum

import           Data.Time (defaultTimeLocale, parseTimeOrError)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant.API.Generic (toServant)
import           Servant.Server (Handler, Server, serve)
import           Servant.Server.Generic (AsServerT)

import           Pos.Core (EpochIndex (..), mkCoin)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api (ExplorerApi, ExplorerApiRecord (..),
                     explorerApi)
import           Pos.Explorer.Web.ClientTypes (Byte, CAda (..), CAddress (..),
                     CAddressSummary (..), CAddressType (..),
                     CAddressesFilter (..), CBlockEntry (..),
                     CBlockSummary (..), CGenesisAddressInfo (..),
                     CGenesisSummary (..), CHash (..), CTxBrief (..),
                     CTxEntry (..), CTxId (..), CTxSummary (..), CUtxo (..),
                     mkCCoin)
import           Pos.Explorer.Web.Error (ExplorerError (..))
import           Pos.Web ()


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
    toServant (ExplorerApiRecord
        { _totalAda           = testTotalAda
        , _blocksPages        = testBlocksPages
        , _blocksPagesTotal   = testBlocksPagesTotal
        , _blocksSummary      = testBlocksSummary
        , _blocksTxs          = testBlocksTxs
        , _txsLast            = testTxsLast
        , _txsSummary         = testTxsSummary
        , _addressSummary     = testAddressSummary
        , _addressUtxoBulk    = testAddressUtxoBulk
        , _epochPages         = testEpochPageSearch
        , _epochSlots         = testEpochSlotSearch
        , _genesisSummary     = testGenesisSummary
        , _genesisPagesTotal  = testGenesisPagesTotal
        , _genesisAddressInfo = testGenesisAddressInfo
        , _statsTxs           = testStatsTxs
        }
        :: ExplorerApiRecord (AsServerT Handler))

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

cTxId :: CTxId
cTxId = CTxId $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

cTxEntry :: CTxEntry
cTxEntry = CTxEntry
    { cteId         = cTxId
    , cteTimeIssued = Just posixTime
    , cteAmount     = mkCCoin $ mkCoin 33333
    }

cTxBrief :: CTxBrief
cTxBrief = CTxBrief
    { ctbId         = cTxId
    , ctbTimeIssued = Just posixTime
    , ctbInputs     = [Just (CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv", mkCCoin $ mkCoin 33333), Nothing]
    , ctbOutputs    = [(CAddress "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ", mkCCoin $ mkCoin 33333)]
    , ctbInputSum   = mkCCoin $ mkCoin 33333
    , ctbOutputSum  = mkCCoin $ mkCoin 33333
    }

----------------------------------------------------------------
-- Test handlers
----------------------------------------------------------------

testTotalAda :: Handler CAda
testTotalAda = pure $ CAda 123.456789

testBlocksPagesTotal
    :: Maybe Word
    -> Handler Integer
testBlocksPagesTotal _ = pure 10

testBlocksPages
    :: Maybe Word
    -> Maybe Word
    -> Handler (Integer, [CBlockEntry])
testBlocksPages _ _  = pure (1, [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Just posixTime
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin $ mkCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    , cbeFees       = mkCCoin $ mkCoin 0
    }])

testBlocksSummary
    :: CHash
    -> Handler CBlockSummary
testBlocksSummary _ = pure CBlockSummary
    { cbsEntry      = CBlockEntry
                        { cbeEpoch      = 37294
                        , cbeSlot       = 10
                        , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
                        , cbeTimeIssued = Just posixTime
                        , cbeTxNum      = 0
                        , cbeTotalSent  = mkCCoin $ mkCoin 0
                        , cbeSize       = 390
                        , cbeBlockLead  = Nothing
                        , cbeFees       = mkCCoin $ mkCoin 0
                        }
    , cbsPrevHash   = CHash "d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e"
    , cbsNextHash   = Just (CHash "d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1")
    , cbsMerkleRoot = CHash "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
    }

testBlocksTxs
    :: CHash
    -> Maybe Word
    -> Maybe Word
    -> Handler [CTxBrief]
testBlocksTxs _ _ _ = pure [cTxBrief]

testTxsLast :: Handler [CTxEntry]
testTxsLast = pure [cTxEntry]

testTxsSummary
    :: CTxId
    -> Handler CTxSummary
testTxsSummary _       = pure CTxSummary
    { ctsId              = CTxId $ CHash "8aac4a6b18fafa2783071c66519332157ce96c67e88fc0cc3cb04ba0342d12a1"
    , ctsTxTimeIssued    = Just posixTime
    , ctsBlockTimeIssued = Nothing
    , ctsBlockHeight     = Just 13
    , ctsBlockEpoch      = Just 0
    , ctsBlockSlot       = Just 13
    , ctsBlockHash       = Just $ CHash "a9dea19829e80d9064cd0c33dccf5369638e43c62a090848342037e296120a35"
    , ctsRelayedBy       = Nothing
    , ctsTotalInput      = mkCCoin $ mkCoin 33333
    , ctsTotalOutput     = mkCCoin $ mkCoin 33333
    , ctsFees            = mkCCoin $ mkCoin 0
    , ctsInputs          =  [ Just (CAddress "19HxN7PseAPT93RftAh7bBmbnJU5gtH6QzvUyZXnbz9Y1UtYwPDdiCGkB2gwvC8CjBUtHXBij9j9Qb6JYgHPi6LtevDcFQ", mkCCoin $ mkCoin 97)
                            , Nothing
                            , Just (CAddress "LaVWPVaMHxNVtqJ1uvVZ8FyQmeRam5avHE1Uv9iwRivCKTN83CUW", mkCCoin $ mkCoin 3333)
                            ]
    , ctsOutputs         =  [ (CAddress "19F6U1Go5B4KakVoCZfzCtqNAWhUBprxVzL3JsGu74TEwQnXPvAKPUbvG8o4Qe5RaY8Z7WKLfxmNFwBqPV1NQ2hRpKkdEN", mkCCoin $ mkCoin 94)
                            , (CAddress "1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8", mkCCoin $ mkCoin 3)
                            ]
    }

testAddressSummary
    :: CAddress
    -> Handler CAddressSummary
testAddressSummary _  = pure sampleAddressSummary

testAddressUtxoBulk
    :: [CAddress]
    -> Handler [CUtxo]
testAddressUtxoBulk _  = pure [CUtxo
    { cuId = CTxId $ CHash "8aac4a6b18fafa2783071c66519332157ce96c67e88fc0cc3cb04ba0342d12a1"
    , cuOutIndex = 0
    , cuAddress = CAddress "19F6U1Go5B4KakVoCZfzCtqNAWhUBprxVzL3JsGu74TEwQnXPvAKPUbvG8o4Qe5RaY8Z7WKLfxmNFwBqPV1NQ2hRpKkdEN"
    , cuCoins = mkCCoin $ mkCoin 3
    }]

testEpochSlotSearch
    :: EpochIndex
    -> Word16
    -> Handler [CBlockEntry]
-- `?epoch=1&slot=1` returns an empty list
testEpochSlotSearch (EpochIndex 1) 1 =
    pure []
-- `?epoch=1&slot=2` returns an error
testEpochSlotSearch (EpochIndex 1) 2 =
    throwM $ Internal "Error while searching epoch/slot"
-- all others returns a simple result
testEpochSlotSearch _ _ = pure [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Just posixTime
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin $ mkCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    , cbeFees       = mkCCoin $ mkCoin 0
    }]

testEpochPageSearch
    :: EpochIndex
    -> Maybe Int
    -> Handler (Int, [CBlockEntry])
testEpochPageSearch _ _ = pure (1, [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Just posixTime
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin $ mkCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    , cbeFees       = mkCCoin $ mkCoin 0
    }])

testGenesisSummary
    :: Handler CGenesisSummary
testGenesisSummary = pure CGenesisSummary
    { cgsNumTotal       = 4
    , cgsNumRedeemed    = 3
    , cgsNumNotRedeemed = 1
    , cgsRedeemedAmountTotal    = mkCCoin $ mkCoin 300000000
    , cgsNonRedeemedAmountTotal = mkCCoin $ mkCoin 100000000
    }

-- mock CGenesisAddressInfo
gAddressInfoA :: CGenesisAddressInfo
gAddressInfoA = CGenesisAddressInfo
    { cgaiCardanoAddress   = CAddress "3mfaPhQ8ewtmyi7tvcxo1TXhGh5piePbjkqgz49Jo2wpV9"
    , cgaiGenesisAmount    = mkCCoin $ mkCoin 2225295000000
    , cgaiIsRedeemed       = True
    }

-- mock another CGenesisAddressInfo
gAddressInfoB :: CGenesisAddressInfo
gAddressInfoB = CGenesisAddressInfo
    { cgaiCardanoAddress   = CAddress "3meLwrCDE4C7RofEdkZbUuR75ep3EcTmZv9ebcdjfMtv5H"
    , cgaiGenesisAmount    = mkCCoin $ mkCoin 15000000
    , cgaiIsRedeemed       = False
    }

-- mock another CGenesisAddressInfo
gAddressInfoC :: CGenesisAddressInfo
gAddressInfoC = CGenesisAddressInfo
    { cgaiCardanoAddress   = CAddress "LaVWbkFegK1TUNHMc3Fads2cG6ivPb2gJUxXBxNtumLtbG"
    , cgaiGenesisAmount    = mkCCoin $ mkCoin 333000000
    , cgaiIsRedeemed       = False
    }

testGenesisPagesTotal
    :: Maybe Word
    -> CAddressesFilter
    -> Handler Integer
-- number of redeemed addresses pages
testGenesisPagesTotal _ RedeemedAddresses    = pure 1
-- number of non redeemed addresses pages
testGenesisPagesTotal _ NonRedeemedAddresses = pure 1
-- number of all redeem addresses pages
testGenesisPagesTotal _ _                    = pure 2

testGenesisAddressInfo
    :: Maybe Word
    -> Maybe Word
    -> CAddressesFilter
    -> Handler [CGenesisAddressInfo]
-- filter redeemed addresses
testGenesisAddressInfo _ _ RedeemedAddresses =
    pure [ gAddressInfoA ]
-- filter non-redeemed addresses
testGenesisAddressInfo _ _ NonRedeemedAddresses =
    pure [ gAddressInfoB, gAddressInfoC ]
-- all addresses (w/o filtering) - page 1
testGenesisAddressInfo (Just 1) _ AllAddresses =
    pure [ gAddressInfoA, gAddressInfoB ]
-- all addresses (w/o filtering) - page 2
testGenesisAddressInfo (Just 2) _ AllAddresses =
    pure [ gAddressInfoC ]
-- all others requests will ended up with an error
testGenesisAddressInfo _ _ _ =
    throwM $ Internal "Error while pagening genesis addresses"

testStatsTxs
    :: Maybe Word
    -> Handler (Integer, [(CTxId, Byte)])
testStatsTxs _ = pure (1, [(cTxId, 200)])
