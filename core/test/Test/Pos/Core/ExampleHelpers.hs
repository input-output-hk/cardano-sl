module Test.Pos.Core.ExampleHelpers
       (  -- Example data

          exampleAddrSpendingData_PubKey
        , exampleAddress
        , exampleAddress1
        , exampleAddress2
        , exampleAddress3
        , exampleAddress4
        , exampleAddress5
        , exampleAddress6
        , exampleAddress7
        , exampleAddress'
        , exampleAddress'1
        , exampleAddress'2
        , exampleAddress'3
        , exampleAddress'4
        , exampleAddress'5
        , exampleAddress'6
        , exampleAddress'7
        , exampleAttributes
        , exampleChainDifficulty
        , exampleEpochIndex
        , examplePublicKey
        , examplePublicKeys
        , exampleRedeemPublicKey
        , exampleSafeSigner
        , exampleScript
        , exampleSecretKey
        , exampleSecretKeys
        , exampleSharedSeed0
        , exampleSharedSeed1
        , exampleSharedSeed2
        , exampleSlotId
        , exampleSlottingData
        , exampleSlotLeaders
        , exampleStakeholderId
        , exampleStakeholderIds
        , exampleStakesList
        , exampleVssPublicKeys
        , staticSafeSigners

        -- Helpers
        , feedPM
        , feedPMWithRequiresMagic
        , feedPC
        , feedPMC
        , feedEpochSlots
        , feedPMEpochSlots
        , getText
       ) where

import           Universum

import           Data.List ((!!))
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Hedgehog as H

import qualified Cardano.Crypto.Wallet as CC
import           Pos.Core.Attributes (Attributes, mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), Address (..), Address',
                     BlockCount (..), ChainDifficulty (..), Coin (..),
                     CoinPortion (..), Script (..), SharedSeed (..),
                     SlotLeaders, StakeholderId, StakesList,
                     coinPortionDenominator, makeAddress, makeAddress',
                     mkMultiKeyDistr)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Core.ProtocolConstants (ProtocolConstants, pcEpochSlots)
import           Pos.Core.Slotting (EpochIndex (..), EpochSlottingData (..),
                     LocalSlotIndex (..), SlotCount, SlotId (..), SlottingData,
                     createSlottingDataUnsafe)
import           Pos.Crypto (HDAddressPayload (..), ProtocolMagic (..),
                     RedeemPublicKey, RequiresNetworkMagic (..),
                     SafeSigner (..), SecretKey (..), VssPublicKey (..),
                     abstractHash, deterministicVssKeyGen,
                     redeemDeterministicKeyGen, toVssPublicKey)
import           Pos.Crypto.Signing (PublicKey (..))

import           Test.Pos.Core.Gen (genProtocolConstants)
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Crypto.Gen (genProtocolMagic, genProtocolMagicId)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic

feedPMWithRequiresMagic :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPMWithRequiresMagic genA = do
    pm <- flip ProtocolMagic RequiresMagic <$> genProtocolMagicId
    genA pm

feedPC :: (ProtocolConstants -> H.Gen a) -> H.Gen a
feedPC genA = genA =<< genProtocolConstants

feedPMC :: (ProtocolMagic -> ProtocolConstants -> H.Gen a) -> H.Gen a
feedPMC genA = do
    pm <- genProtocolMagic
    pc <- genProtocolConstants
    genA pm pc

feedEpochSlots :: (SlotCount -> H.Gen a) -> H.Gen a
feedEpochSlots genA = genA =<< pcEpochSlots <$> genProtocolConstants

feedPMEpochSlots :: (ProtocolMagic -> SlotCount -> H.Gen a) -> H.Gen a
feedPMEpochSlots genA = do
    pm <- genProtocolMagic
    epochSlots <- pcEpochSlots <$> genProtocolConstants
    genA pm epochSlots

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners!!offset

exampleStakeholderId :: StakeholderId
exampleStakeholderId = abstractHash examplePublicKey :: StakeholderId

exampleStakeholderIds :: Int -> Int -> [StakeholderId]
exampleStakeholderIds offset l = map abstractHash $ examplePublicKeys offset l

exampleVssPublicKeys :: Int -> Int -> [VssPublicKey]
exampleVssPublicKeys offset count = map (toKey . (*offset)) [0..count]
    where
        toKey start = toVssPublicKey . deterministicVssKeyGen $ (getBytes start 32)

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePublicKey

examplePublicKey :: PublicKey
examplePublicKey = pk
  where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right pk = PublicKey <$> CC.xpub (getBytes start 64)
                   in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey = fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = (exampleSecretKeys 10 2) !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right sk = SecretKey <$> CC.xprv (getBytes start 128)
                   in sk

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakesList :: StakesList
exampleStakesList = zip sis coins
  where
    sis   = map abstractHash (examplePublicKeys 15 3)
    coins = map Coin [79, 44, 9999999]

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = map abstractHash (examplePublicKeys 16 3)

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map FakeSigner (exampleSecretKeys 1 6)

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
staticText :: Text
staticText
    = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

getText :: Int -> Int -> Text
getText offset len = T.take len $ T.drop offset staticText

exampleAddress :: Address
exampleAddress = makeAddress exampleAddrSpendingData_PubKey attrs
  where
    attrs = AddrAttributes hap BootstrapEraDistr NetworkMainOrStage
    hap = Just (HDAddressPayload (getBytes 32 32))

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 24 1
    attrs = AddrAttributes hap BootstrapEraDistr NetworkMainOrStage
    hap = Nothing

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
  where
    easd = RedeemASD exampleRedeemPublicKey
    attrs = AddrAttributes hap asd NetworkMainOrStage
    hap = Just (HDAddressPayload (getBytes 15 32))
    asd = SingleKeyDistr exampleStakeholderId

exampleAddress3 :: Address
exampleAddress3 = makeAddress easd attrs
  where
    easd = ScriptASD exampleScript
    attrs = AddrAttributes hap exampleMultiKeyDistr NetworkMainOrStage
    hap = Just (HDAddressPayload (getBytes 17 32))

exampleAddress4 :: Address
exampleAddress4 = makeAddress easd attrs
  where
    easd = UnknownASD 7 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) NetworkMainOrStage
    [sId] = exampleStakeholderIds 7 1

exampleAddress5 :: Address
exampleAddress5 = makeAddress easd attrs
  where
    easd = ScriptASD exampleScript
    attrs = AddrAttributes hap exampleMultiKeyDistr (NetworkTestnet 12345)
    hap = Just (HDAddressPayload (getBytes 10 32))

exampleAddress6 :: Address
exampleAddress6 = makeAddress easd attrs
  where
    easd = UnknownASD 200 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) (NetworkTestnet 31337)
    [sId] = exampleStakeholderIds 10 1

exampleAddress7 :: Address
exampleAddress7 = makeAddress easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 16 1
    attrs = AddrAttributes hap BootstrapEraDistr (NetworkTestnet (- 559038737))
    hap = Nothing

exampleAddress' :: Address'
exampleAddress' = makeAddress' exampleAddrSpendingData_PubKey attrs
  where
    attrs = AddrAttributes hap BootstrapEraDistr NetworkMainOrStage
    hap = Just (HDAddressPayload (getBytes 32 32))

exampleAddress'1 :: Address'
exampleAddress'1 = makeAddress' easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 24 1
    attrs = AddrAttributes hap BootstrapEraDistr NetworkMainOrStage
    hap = Nothing

exampleAddress'2 :: Address'
exampleAddress'2 = makeAddress' easd attrs
  where
    easd = RedeemASD exampleRedeemPublicKey
    attrs = AddrAttributes hap asd NetworkMainOrStage
    hap = Just (HDAddressPayload (getBytes 15 32))
    asd = SingleKeyDistr exampleStakeholderId

exampleAddress'3 :: Address'
exampleAddress'3 = makeAddress' easd attrs
  where
    easd = ScriptASD exampleScript
    attrs = AddrAttributes hap exampleMultiKeyDistr NetworkMainOrStage
    hap = Just (HDAddressPayload (getBytes 17 32))

exampleAddress'4 :: Address'
exampleAddress'4 = makeAddress' easd attrs
  where
    easd = UnknownASD 7 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) NetworkMainOrStage
    [sId] = exampleStakeholderIds 7 1

exampleAddress'5 :: Address'
exampleAddress'5 = makeAddress' easd attrs
  where
    easd = ScriptASD exampleScript
    attrs = AddrAttributes hap exampleMultiKeyDistr (NetworkTestnet 12345)
    hap = Just (HDAddressPayload (getBytes 10 32))

exampleAddress'6 :: Address'
exampleAddress'6 = makeAddress' easd attrs
  where
    easd = UnknownASD 200 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) (NetworkTestnet 31337)
    [sId] = exampleStakeholderIds 10 1

exampleAddress'7 :: Address'
exampleAddress'7 = makeAddress' easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 16 1
    attrs = AddrAttributes hap BootstrapEraDistr (NetworkTestnet (- 559038737))
    hap = Nothing

exampleMultiKeyDistr :: AddrStakeDistribution
exampleMultiKeyDistr = case mkMultiKeyDistr (M.fromList pairs) of
    Left err -> error $
        "exampleMultiKeyDistr: improperly constructed stake map: " <> show err
    Right asd -> asd
  where
    pairs = zip stakeIds (map CoinPortion (remainderCP : coinPortions))
    stakeIds = map abstractHash (examplePublicKeys 7 4)
    coinPortions = [ (10 :: Word64) ^ (12 :: Word64)
                   , ( 7 :: Word64) ^ (11 :: Word64)
                   , ( 6 :: Word64) ^ (14 :: Word64)
                   ]
    remainderCP = coinPortionDenominator - sum coinPortions

exampleSharedSeed0 :: SharedSeed
exampleSharedSeed0 = SharedSeed (getBytes 8 32)

exampleSharedSeed1 :: SharedSeed
exampleSharedSeed1 = SharedSeed (getBytes 16 32)

exampleSharedSeed2 :: SharedSeed
exampleSharedSeed2 = SharedSeed (getBytes 24 32)

exampleSlottingData :: SlottingData
exampleSlottingData =
  createSlottingDataUnsafe
    $   M.fromList
    $   (,)
    <$> [0 .. 9]
    <*> pure exampleEpochSlottingData

exampleEpochSlottingData :: EpochSlottingData
exampleEpochSlottingData = EpochSlottingData
    { esdSlotDuration = 100
    , esdStartDiff = 100
    }
