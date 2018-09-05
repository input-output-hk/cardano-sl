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
import           Servant.Generic (AsServerT, toServant)
import           Servant.Server (Handler, Server, serve)

import           Pos.Core (EpochIndex (..), mkCoin)
import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Web.Api (ExplorerApi, ExplorerApiRecord (..),
                     explorerApi)
import           Pos.Explorer.Web.ClientTypes (Byte, CAda (..), CAddress (..),
                     CAddressSummary (..), CAddressType (..),
                     CAddressesFilter (..), CBlockEntry (..),
                     CBlockSummary (..), CByteString (..),
                     CGenesisAddressInfo (..), CGenesisSummary (..),
                     CHash (..), CTxBrief (..), CTxEntry (..), CTxId (..),
                     CTxSummary (..), CUtxo (..), mkCCoin)
import           Pos.Explorer.Web.Error (ExplorerError (..))
import           Pos.Web ()

import qualified Data.ByteString.Char8


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
        , _txsRaw             = testTxsRaw
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

testTxsRaw
    :: CTxId
    -> Handler CByteString
testTxsRaw _ = (pure . CByteString . Data.ByteString.Char8.pack) "839f8200d81858248258201e561e7a214b163cf530975ab0cc161c29cb20f7c10304c5ddb9fa1409fe699f00ff9f8282d818584283581cb7ad59fbfbb34df88f8e3a93ebec03e1a5077ae79df01a040f31366fa101581e581cea1425ccdd649bd63454c77eecdbee20a64f23299deb0b02a0e84b8c001a380603af1b000001e46cca151c8282d818584283581cc0ed966738a3b8b76d2614990085ab949c75dd0df6e868a6ccb2c686a101581e581ca8a675510be4634625797448bbd6379cac7cadf95dd7c4a67e33e307001aa83081341a73846ef88282d818584283581c4402b0c630941bb4bc10709a0e9a4f0aad94fa6a1e7b60164bfbe29ba101581e581cca3e553c9c63c59a87f14643a43e451f991d6d42758801be8f7aa31e001a3cdb64ed1b0000000325262a008282d818584283581c4f386e763eeeee0dfd3790ec17d67260e20a220059c63dbd92a34c38a101581e581c5fcd73e9878694d94863314b29aeadbc8768261900dabff229bb8db5001aa8ef1c221b00000007210e30088282d818584283581c964af58fcce68f39b2d3b54d63543cb5b6c203d043c3576b3671c6bfa101581e581c07a92009757f32c555d9a956151b0fcf1189b4a6f8d5013fab8331f7001a5d955a901ab3e306808282d818584283581c479448bb68428d8ccc9fe24ec84f901a32cdafafa45b215be76725aaa101581e581cd22ea3c67fbc162d2bb57fcaa4ec7dbf1ef112055a06f7f9794d9524001a56ded1ad1a5958ecc08282d818584283581c4be3f7ceae80b3c0f4dc7e12b8451c22b0452a568a5b90bf60f63750a101581e581c80f5433d7d19cc295eb1990fd01b82f850f4e48207174909896fd347001a767c443d1a407cba608282d818584283581c81fbbcfd9985c5575980495785d1a0d0a6f35bdf48a6c6fe780d7548a101581e581c1afbc57540db15064d2554d72fe315cf88eef90fdeae89f66824d2b4001a7c4ab90a1b0000003710a4788c8282d818584283581c30997d66ff9fce30e6d1c20b8222f1f7776d797c8fa6d93bf81eac60a101581e581cb57aa09da0689daaa1d0ae28fa882a2e888262eb65a33e4a571df434001aad291a8c1a03ca460a8282d818584283581cd0f720912201573f8e41c92ebb442d6934554c98b968484c953d94fea101581e581cc306c8ccb0bbe3dfc011cd50d78260c50c67123f50590c6c3faa4574001a0a8233361a483a63108282d818584283581c030a0bddb9065021c475d458414061268a149470cd5ea2b8af0c153aa101581e581c1afbc57540db152f7e0fe5d79c208b5eb8bc9873c91dbc05ef25a4db001a8a72c45a1b000000114231bd408282d818584283581c70f5e6700e0c8d0f7c26dab17f8e0e7e77f9169e11b62639bd26b3b6a101581e581c1afbc57540db156c73563cd7e1f68ffc858a6e2202d443ef7095ae78001a72d463a01b00000035e4fc32c08282d818584283581cb075829b980e4c1bf3b904ccde7af228276d5d556df15b975ff00613a101581e581cfa50263d66a37ff2b1098a63309bd44da1f82ee207d95bd30e6caa65001a0ef9f7f41ad0ad05408282d818584283581c16eb1cdbc3478d8ccd0b67e082ecf6c65dbcc4b6bf2f869b55d6c8d5a101581e581c1afbc57540db150ff59fa1d772ac06384f8f1cd26141e1e621a889ac001a0a04a8e11b00000009ae8a39c08282d818584283581cac0251cbb896f5a4704154b7c371b1c9f9f679baf7fe01deb7baf270a101581e581c8ee46672b9149bea9155d66be2da24ae429b7b60b5c7c787887d4afc001aaea79fee1a826750e88282d818584283581cd9d37a1d6ccd3769123c7d8fd2cb7a5c31a06d9874d8fdde67853b91a101581e581cd9273503e469fcb4ba09b3cca9ee29eef3d05170f2693163e4343e7f001a581e45921b0000000e040553408282d818584283581c64c24a0da3d88e4794d14ac204a3c3c2d20bf832f08f8211390b00c3a101581e581c0cc34d7bd202e52df6af0d91a637f25421d4788a04bfd28640b93cb6001a431473bd1a3c70bf708282d818584283581c4402b0c630941bb4bc10709a0e9a4f0aad94fa6a1e7b60164bfbe29ba101581e581cca3e553c9c63c59a87f14643a43e451f991d6d42758801be8f7aa31e001a3cdb64ed1b0000000348da2dc08282d818584283581c795b9e864b3fb4d3787d4da0826e6483f2c52122a2f21b7204dc9bcfa101581e581ceeb3c063adf8512676b52ee2eb37ab25c8127535e2a6f9423ff73275001ae490d81a1a002dc6c08282d818584283581cab491c526cad1e57362bb9c62b093cf61c17e0284cd207982804da3ea101581e581c1afbc57540db155d5ffd05d71436980ee276f9516a1b5c629af7cfef001a10b2a22f1b000000054b781f408282d818584283581c22098f84101500742c5a50b19d6f8ef009ed30be9f7a4956c1c8b6c4a101581e581c1afbc57540db151018b369d77d181f6e91d006da286e710785f03d50001a768873ba1b000000174867a5c08282d818584283581c9791d0211a9eb2d14aa12e7b2d289bb64a97653460b3f55001fecdbba101581e581c1afbc57540db156857055cd77f3fef88f8fa46237d9a0c1840e6268d001ade7110f01ad5c442288282d818584283581c86cb38f2c345504e9a6a1ba6a8eeb7dbd5724b4ab8a00d756855fd57a101581e581c1afbc57540db1500407c02d79829f68cb9e73177c0cf44d887414802001ae347929b1a5a528f008282d818584283581cecf1951fdbcc4925cf2f3c10296b63a05716f53d28960808be6f8ff7a101581e581c4e4f8ff4439731babd2edbd0df621f06b3453152a5f965ddcb32b472001a1fb201dc1a270fbd988282d818584283581cdc4d40b2971494c216974639a71feddaa4fe32f0c95e12c0f7460246a101581e581ce3977c040561ddd7f1b523db4bb716342b8f698543478e4932fa5f90001a1cdb21a71a159d62e08282d818584283581cef4e83e0ad2bb29f5ff4d02fd22404cf21f6946c61c1aa80fa11fab4a101581e581c1afbc57540db155199ed7ed7806c319d9ddbb508272d97af6c94cd20001a8cb711ae1b00000010fa15944bffa0"

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
