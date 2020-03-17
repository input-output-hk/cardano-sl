module Test.Pos.Core.Gen
       (
        -- Pos.Core.Common Generators
          genAddrAttributes
        , genAddress
        , genAddrSpendingData
        , genAddrStakeDistribution
        , genAddrType
        , genBlockCount
        , genChainDifficulty
        , genCoeff
        , genCoin
        , genCoinPortion
        , genScript
        , genScriptVersion
        , genSharedSeed
        , genSlotLeaders
        , genStakeholderId
        , genStakesList
        , genStakesMap
        , genTxFeePolicy
        , genTxSizeLinear

        -- Pos.Core.JsonLog Generators
        , genInvReqDataFlowLog

        -- Pos.Core.ProtocolConstants
        , genProtocolConstants
        , genVssMaxTTL
        , genVssMinTTL

        -- Pos.Core.Slotting Generators
        , genEpochIndex
        , genEpochOrSlot
        , genEpochSlottingData
        , genFlatSlotId
        , genLocalSlotIndex
        , genSlotCount
        , genSlotId
        , genSlottingData
        , genTimeDiff
        , genTimestamp
        , genTimestampRoundedToSecond

        -- Pos.Core.Attributes Generators
        , genAttributes

        -- Pos.Merkle Generators
        , genMerkleRoot
        , genMerkleTree
        , genHashRaw

        -- Helpers
        , genTextHash
        , genByte
        , genBytes
        , genUTF8Byte
        , genWord16
        , genWord32
        , gen32Bytes
        , genMillisecond
       ) where

import           Universum

import           Data.Fixed (Fixed (..))
import qualified Data.Map as M
import           Data.Time.Units (Microsecond, Millisecond, fromMicroseconds)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pos.Binary.Class (Bi, Raw (..))
import           Pos.Core.Attributes (Attributes (..), mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), AddrType (..), Address (..),
                     BlockCount (..), ChainDifficulty (..), Coeff (..),
                     Coin (..), CoinPortion (..), Script (..), ScriptVersion,
                     SharedSeed (..), SlotLeaders, StakeholderId, StakesList,
                     StakesMap, TxFeePolicy (..), TxSizeLinear (..),
                     coinPortionDenominator, makeAddress, maxCoinVal,
                     mkMultiKeyDistr)
import           Pos.Core.JsonLog.LogEvents (InvReqDataFlowLog (..))
import           Pos.Core.Merkle (MerkleRoot (..), MerkleTree (..),
                     mkMerkleTree, mtRoot)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                     EpochSlottingData (..), FlatSlotId, LocalSlotIndex (..),
                     SlotCount (..), SlotId (..), SlottingData, TimeDiff (..),
                     Timestamp (..), createSlottingDataUnsafe,
                     localSlotIndexMaxBound, localSlotIndexMinBound)
import           Pos.Crypto (Hash, hash)
import           Pos.Util.Util (leftToPanic)
import           Serokell.Data.Memory.Units (Byte)

import           Test.Pos.Crypto.Gen (genAbstractHash, genHDAddressPayload,
                     genPublicKey, genRedeemPublicKey)
import           Test.Pos.Util.Gen (genHashMap)

----------------------------------------------------------------------------
-- Pos.Core.Common Generators
----------------------------------------------------------------------------

genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = AddrAttributes <$> hap <*> genAddrStakeDistribution <*> nm
  where
    hap = Gen.maybe genHDAddressPayload
    nm  = Gen.choice [ pure NetworkMainOrStage
                     , NetworkTestnet <$> genInt32
                     ]

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddrType :: Gen AddrType
genAddrType = Gen.choice [ pure ATPubKey
                         , pure ATScript
                         , pure ATRedeem
                         -- Values 0,1,2 are reserved, as they are used to tag
                         -- the above 3 constructors --------------+
                         --                                        |
                         , ATUnknown <$> Gen.word8 (Range.constant 3 maxBound)
                         ]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData = Gen.choice gens
  where
    gens = [ PubKeyASD <$> genPublicKey
           , ScriptASD <$> genScript
           , RedeemASD <$> genRedeemPublicKey
           -- Values 0,1,2 are reserved, as they are used to tag
           -- the above 3 constructors ---------------+
           --                                         |
           , UnknownASD <$> Gen.word8 (Range.constant 3 maxBound) <*> gen32Bytes
           ]

genAddrStakeDistribution :: Gen AddrStakeDistribution
genAddrStakeDistribution = Gen.choice gens
  where
    gens = [ pure BootstrapEraDistr
           , SingleKeyDistr <$> genStakeholderId
           , leftToPanic "arbitrary @AddrStakeDistribution: " .
             mkMultiKeyDistr <$>
             genMultiKeyDistr
           ]

    -- Lifted from `Pos.Arbitrary.Core`. There are very particular constraints
    -- on the AddrStakeDistribution, which are mixed into encoding/decoding.
    genMultiKeyDistr :: Gen (Map StakeholderId CoinPortion)
    -- We don't want to generate too much, hence 'scale'.
    genMultiKeyDistr =
        Gen.scale (`mod` 16) $ do
            holder0 <- genStakeholderId
            holder1 <- Gen.filter (/= holder0) genStakeholderId
            moreHolders <- Gen.list (Range.linear 0 100) genStakeholderId
            -- Must be at least 2 non-repeating stakeholders.
            let holders = ordNub (holder0 : holder1 : moreHolders)
            portions <- genPortions (length holders) []
            return $ M.fromList $ holders `zip` portions
    genPortions :: Int -> [CoinPortion] -> Gen [CoinPortion]
    genPortions 0 res = pure res
    genPortions n res = do
        let limit =
                foldl' (-) coinPortionDenominator $
                map getCoinPortion res
        case (n, limit) of
            -- Limit is exhausted, can't create more.
            (_, 0) -> return res
            -- The last portion, we must ensure the sum is correct.
            (1, _) -> return (CoinPortion limit : res)
            -- We intentionally don't generate 'limit', because we
            -- want to generate at least 2 portions.  However, if
            -- 'limit' is 1, we will generate 1, because we must
            -- have already generated one portion.
            _ -> do
                portion <-
                    CoinPortion <$> Gen.word64 (Range.linear 1 (max 1 (limit - 1)))
                genPortions (n - 1) (portion : res)

genBlockCount :: Gen BlockCount
genBlockCount = BlockCount <$> Gen.word64 Range.constantBounded

genChainDifficulty :: Gen ChainDifficulty
genChainDifficulty = ChainDifficulty <$> genBlockCount

genCoeff :: Gen Coeff
genCoeff = do
    -- A `Coeff` wraps a Nano-precision integral value, which corresponds to a
    -- number of "Lovelace" (10^6 Lovelace == 1 ADA). The `Coeff` values used
    -- in Cardano correspond to less than 1 ADA.
    let exponent = 9 + 6 :: Integer
    integer <- Gen.integral (Range.constant 0 (10^exponent))
    pure $ Coeff (MkFixed integer)

genCoin :: Gen Coin
genCoin = Coin <$> Gen.word64 (Range.constant 0 maxCoinVal)

genCoinPortion :: Gen CoinPortion
genCoinPortion =
    CoinPortion <$> Gen.word64 (Range.constant 0 coinPortionDenominator)

genScript :: Gen Script
genScript = Script <$> genScriptVersion <*> gen32Bytes

genScriptVersion :: Gen ScriptVersion
genScriptVersion = Gen.word16 Range.constantBounded

genSharedSeed :: Gen SharedSeed
genSharedSeed = SharedSeed <$> gen32Bytes

genSlotLeaders :: Gen SlotLeaders
genSlotLeaders = Gen.list (Range.linear 1 10) genStakeholderId

genStakeholderId :: Gen StakeholderId
genStakeholderId = genAbstractHash genPublicKey

genStakesList :: Gen StakesList
genStakesList = Gen.list range gen
  where
    gen = (,) <$> genStakeholderId <*> genCoin
    range = Range.linear 0 10

genStakesMap :: Gen StakesMap
genStakesMap = genHashMap (Range.linear 0 10) genStakeholderId genCoin

genTxFeePolicy :: Gen TxFeePolicy
genTxFeePolicy =
    Gen.choice [ TxFeePolicyTxSizeLinear <$> genTxSizeLinear
               , TxFeePolicyUnknown <$> genUnknownPolicy <*> genUTF8Byte
               ]
  where
    -- 0 is a reserved policy, so we go from 1 to max.
    -- The Bi instance decoder for TxFeePolicy consolidates the
    -- tag and the policy number, so a 0 policy in TxFeePolicyUnknown
    -- causes a decoder error.
    genUnknownPolicy :: Gen Word8
    genUnknownPolicy = Gen.word8 (Range.constant 1 maxBound)

genTxSizeLinear :: Gen TxSizeLinear
genTxSizeLinear = TxSizeLinear <$> genCoeff <*> genCoeff

----------------------------------------------------------------------------
-- Pos.Core.JsonLog Generators
----------------------------------------------------------------------------

genInvReqDataFlowLog :: Gen InvReqDataFlowLog
genInvReqDataFlowLog = Gen.choice
    [ InvReqAccepted
          <$> Gen.integral (Range.constant 1 50)
          <*> Gen.integral (Range.constant 1 50)
          <*> Gen.integral (Range.constant 1 50)
          <*> Gen.integral (Range.constant 1 50)
    , InvReqRejected
          <$> Gen.integral (Range.constant 1 50)
          <*> Gen.integral (Range.constant 1 50)
    , InvReqException <$> Gen.text (Range.constant 1 20) Gen.alphaNum
    ]

----------------------------------------------------------------------------
-- Pos.Core.ProtocolConstants Generators
----------------------------------------------------------------------------

genProtocolConstants :: Gen ProtocolConstants
genProtocolConstants = do
    vssA <- genWord32
    vssB <- genWord32
    let (vssMin, vssMax) = if vssA > vssB
                           then (VssMinTTL vssB, VssMaxTTL vssA)
                           else (VssMinTTL vssA, VssMaxTTL vssB)
    ProtocolConstants <$> Gen.int (Range.constant 1 20000) <*> pure vssMin <*> pure vssMax

genVssMaxTTL :: Gen VssMaxTTL
genVssMaxTTL = VssMaxTTL <$> genWord32

genVssMinTTL :: Gen VssMinTTL
genVssMinTTL = VssMinTTL <$> genWord32

----------------------------------------------------------------------------
-- Pos.Core.Slotting Generators
----------------------------------------------------------------------------

genEpochIndex :: Gen EpochIndex
genEpochIndex = EpochIndex <$> Gen.word64 Range.constantBounded

genEpochOrSlot :: SlotCount -> Gen EpochOrSlot
genEpochOrSlot epochSlots =
    Gen.choice [ EpochOrSlot . Left <$> genEpochIndex
               , EpochOrSlot . Right <$> genSlotId epochSlots
               ]

genEpochSlottingData :: Gen EpochSlottingData
genEpochSlottingData = EpochSlottingData <$> genMillisecond <*> genTimeDiff

genFlatSlotId :: Gen FlatSlotId
genFlatSlotId = Gen.word64 Range.constantBounded

genLocalSlotIndex :: SlotCount -> Gen LocalSlotIndex
genLocalSlotIndex epochSlots =
    UnsafeLocalSlotIndex <$> Gen.word16 (Range.constant lb ub)
  where
    lb = getSlotIndex (localSlotIndexMinBound)
    ub = getSlotIndex (localSlotIndexMaxBound epochSlots)

genSlotCount :: Gen SlotCount
genSlotCount = SlotCount <$> Gen.word64 Range.constantBounded

genSlotId :: SlotCount -> Gen SlotId
genSlotId epochSlots = SlotId <$> genEpochIndex <*> genLocalSlotIndex epochSlots

genSlottingData :: Gen SlottingData
genSlottingData = createSlottingDataUnsafe <$> do
    mapSize <- Gen.int $ Range.linear 2 10
    epochSlottingDatas <- Gen.list (Range.singleton mapSize) genEpochSlottingData
    pure $ M.fromList $ zip [0..fromIntegral mapSize - 1] epochSlottingDatas

genTimeDiff :: Gen TimeDiff
genTimeDiff = TimeDiff <$> genMicrosecond

genTimestamp :: Gen Timestamp
genTimestamp = Timestamp <$> genMicrosecond

-- Microseconds are rounded to the nearest second when enc/decoded to/from
-- JSON. So here we round to the nearest 10^6.
genTimestampRoundedToSecond :: Gen Timestamp
genTimestampRoundedToSecond =
    Timestamp . (* 1000000) . (`rem` 1000000) <$> genMicrosecond

----------------------------------------------------------------------------
-- Pos.Core.Attributes Generators
----------------------------------------------------------------------------

genAttributes :: Gen a -> Gen (Attributes a)
genAttributes genA =  mkAttributes <$> genA

----------------------------------------------------------------------------
-- Pos.Merkle Generators
----------------------------------------------------------------------------

genHashRaw :: Gen (Hash Raw)
genHashRaw = genAbstractHash $ Raw <$> gen32Bytes

-- slow
genMerkleTree :: Bi a => Gen a -> Gen (MerkleTree a)
genMerkleTree genA = mkMerkleTree <$> Gen.list (Range.linear 0 10) genA

-- slow
genMerkleRoot :: Bi a => Gen a -> Gen (MerkleRoot a)
genMerkleRoot genA = mtRoot <$> genMerkleTree genA

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------

genBytes :: Int -> Gen ByteString
genBytes n = Gen.bytes (Range.singleton n)

genUTF8Byte :: Gen ByteString
genUTF8Byte = Gen.utf8 (Range.constant 0 64) Gen.alphaNum

genByte :: Gen Byte
genByte = Gen.integral (Range.constant 0 10)

gen32Bytes :: Gen ByteString
gen32Bytes = genBytes 32

genMillisecond :: Gen Millisecond
genMillisecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genMicrosecond :: Gen Microsecond
genMicrosecond = fromMicroseconds <$> Gen.integral (Range.constant 0 1000000)

genWord32 :: Gen Word32
genWord32 = Gen.word32 Range.constantBounded

genWord16 :: Gen Word16
genWord16 = Gen.word16 Range.constantBounded

genTextHash :: Gen (Hash Text)
genTextHash = do
  sampleText <- Gen.text (Range.linear 0 10) Gen.alphaNum
  pure (hash sampleText :: Hash Text)

genInt32 :: Gen Int32
genInt32 = Gen.int32 Range.constantBounded
