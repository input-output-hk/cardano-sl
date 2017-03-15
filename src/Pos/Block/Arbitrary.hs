{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Block.Arbitrary
       ( HeaderAndParams (..)
       , BlockHeaderList (..)
       , SmallTxPayload (..)
       ) where

import           Data.Ix              (range)
import           Data.List.NonEmpty   (nonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (bprint, build, formatToString, (%))
import           Prelude              (Show (..))
import           System.Random        (mkStdGen, randomR)
import           Test.QuickCheck      (Arbitrary (..), Gen, choose, listOf, listOf, oneof,
                                       oneof, vectorOf)
import           Universum

import           Pos.Binary.Class     (Bi, Raw, biSize)
import           Pos.Block.Network    as T
import           Pos.Constants        (epochSlots)
import           Pos.Crypto           (ProxySecretKey, PublicKey, SecretKey,
                                       createProxySecretKey, toPublic)
import           Pos.Data.Attributes  (Attributes (..), mkAttributes)
import           Pos.Ssc.Arbitrary    (SscPayloadDependsOnSlot (..))
import           Pos.Ssc.Class        (Ssc (..), SscHelpersClass)
import           Pos.Txp.Core         (Tx (..), TxDistribution (..), TxPayload, TxWitness,
                                       mkTxPayload)
import qualified Pos.Types            as T
import           Pos.Update.Arbitrary ()
import           Pos.Util.Arbitrary   (makeSmall)

newtype BodyDependsOnConsensus b = BodyDependsOnConsensus
    { genBodyDepsOnConsensus :: T.ConsensusData b -> Gen (T.Body b)
    }

------------------------------------------------------------------------------------------
-- Arbitrary instances for Blockchain related types
------------------------------------------------------------------------------------------

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.BlockSignature ssc) where
    arbitrary = oneof [ T.BlockSignature <$> arbitrary
                      , T.BlockPSignatureEpoch <$> arbitrary
                      , T.BlockPSignatureSimple <$> arbitrary
                      ]

properBlock
    :: ( Arbitrary (T.BHeaderHash b)
       , Arbitrary (BodyDependsOnConsensus b)
       , Arbitrary (T.ConsensusData b)
       , Arbitrary (T.ExtraBodyData b)
       , Arbitrary (T.ExtraHeaderData b)
       , T.Blockchain b)
    => Gen (T.GenericBlock b)
properBlock = do
    (prevBlock, consensus, extra) <- arbitrary
    BodyDependsOnConsensus{..} <- arbitrary
    body <- genBodyDepsOnConsensus consensus
    let proof = T.mkBodyProof body
        header = T.GenericBlockHeader prevBlock proof consensus extra
    T.GenericBlock <$> pure header <*> pure body <*> arbitrary

------------------------------------------------------------------------------------------
-- GenesisBlockchain
------------------------------------------------------------------------------------------

instance Arbitrary (T.GenesisBlockHeader ssc) where
    arbitrary = T.GenericBlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary (T.BodyProof (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisProof <$> arbitrary

instance Arbitrary (T.ConsensusData (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisConsensusData
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (BodyDependsOnConsensus (T.GenesisBlockchain ssc)) where
    arbitrary = pure $ BodyDependsOnConsensus $ \_ -> arbitrary

instance Arbitrary (T.Body (T.GenesisBlockchain ssc)) where
    arbitrary = T.GenesisBody <$> arbitrary

instance Arbitrary (T.GenericBlock (T.GenesisBlockchain ssc)) where
    arbitrary = properBlock

------------------------------------------------------------------------------------------
-- MainBlockchain
------------------------------------------------------------------------------------------

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.MainBlockHeader ssc) where
    arbitrary = T.GenericBlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary h => Arbitrary (Attributes h) where
    arbitrary = Attributes
        <$> arbitrary
        <*> arbitrary

instance Arbitrary T.MainExtraHeaderData where
    arbitrary = T.MainExtraHeaderData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary T.MainExtraBodyData where
    arbitrary = T.MainExtraBodyData <$> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw) =>
    Arbitrary (T.BodyProof (T.MainBlockchain ssc)) where
    arbitrary = T.MainProof
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.ConsensusData (T.MainBlockchain ssc)) where
    arbitrary = T.MainConsensusData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

-- | In the main blockchain's body, the number of transactions must be the same as the
-- number of transaction witnesses.
--
-- Furthermore, for every transaction in index i of the list, the length of its output
-- list must be the same as the length of the i-th item in the TxDistribution list.
--
-- Because of this, the Arbitrary instance for Ssc ssc => Body (MainBlockchain ssc)
-- ensures that for every transaction generated, a transaction witness is generated as
-- well, and the lengths of its list of outputs must also be the same as the length of its
-- corresponding TxDistribution item.

txOutDistGen :: Gen [(Tx, TxWitness, TxDistribution)]
txOutDistGen = listOf $ do
    txInW <- arbitrary
    txIns <- arbitrary
    (txOuts, txDist) <- second TxDistribution . NE.unzip <$> arbitrary
    return (UnsafeTx txIns txOuts $ mkAttributes (), txInW, txDist)

instance Arbitrary TxPayload where
    arbitrary =
        fromMaybe (error "arbitrary@TxPayload: mkTxPayload failed") .
        mkTxPayload <$>
        txOutDistGen

newtype SmallTxPayload =
    SmallTxPayload TxPayload
    deriving (Show, Eq, Bi)

instance Arbitrary SmallTxPayload where
    arbitrary = SmallTxPayload <$> makeSmall arbitrary

instance Arbitrary (SscPayloadDependsOnSlot ssc) =>
         Arbitrary (BodyDependsOnConsensus (T.MainBlockchain ssc)) where
    arbitrary = pure $ BodyDependsOnConsensus $ \T.MainConsensusData{..} -> makeSmall $ do
        txPayload   <- arbitrary
        generator   <- genPayloadDependsOnSlot @ssc <$> arbitrary
        mpcData     <- generator _mcdSlot
        mpcProxySKs <- arbitrary
        mpcUpload   <- arbitrary
        return $ T.MainBody txPayload mpcData mpcProxySKs mpcUpload

instance Arbitrary (SscPayload ssc) => Arbitrary (T.Body (T.MainBlockchain ssc)) where
    arbitrary = makeSmall $ do
        txPayload   <- arbitrary
        mpcData     <- arbitrary
        mpcProxySKs <- arbitrary
        mpcUpload   <- arbitrary
        return $ T.MainBody txPayload mpcData mpcProxySKs mpcUpload

instance (Arbitrary (SscProof ssc), Arbitrary (SscPayloadDependsOnSlot ssc), SscHelpersClass ssc) =>
    Arbitrary (T.GenericBlock (T.MainBlockchain ssc)) where
    arbitrary = properBlock

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = T.MsgGetHeaders
        <$> arbitrary
        <*> arbitrary

instance Arbitrary T.MsgGetBlocks where
    arbitrary = T.MsgGetBlocks
        <$> arbitrary
        <*> arbitrary

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) => Arbitrary (T.MsgHeaders ssc) where
    arbitrary = T.MsgHeaders <$> arbitrary

instance (Arbitrary (SscProof ssc), Arbitrary (SscPayloadDependsOnSlot ssc), SscHelpersClass ssc) =>
    Arbitrary (T.MsgBlock ssc) where
    arbitrary = T.MsgBlock <$> arbitrary

instance T.BiSsc ssc => Buildable (T.BlockHeader ssc, PublicKey) where
    build (block, key) =
        bprint
            ( build%"\n"%
              build%"\n"
            ) block key

newtype BlockHeaderList ssc = BHL
    { getHeaderList :: ([T.BlockHeader ssc], [PublicKey])
    } deriving (Eq)

instance T.BiSsc ssc => Show (BlockHeaderList ssc) where
    show =
        concat . intersperse "\n" . map (formatToString build) . uncurry zip . getHeaderList

-- | Starting Epoch in block header verification tests
startingEpoch :: Integral a => a
startingEpoch = 0

-- | Maximum permitted epoch in block header verification tests
maxEpochs :: Integral a => a
maxEpochs = 0

-- | Generation of arbitrary, valid headerchain along with a list of leaders for
-- each epoch.
--
-- Because `verifyHeaders` assumes the head of the list is the most recent
-- block, this function is tail-recursive: while keeping track of the current
-- block and epoch/slot, it adds the most recent one to the head of the header list
-- it'll return when done.
--
-- The `[Either SecretKey (SecretKey, SecretKey, Bool)]` type is for determining what
-- kind of signature the slot's block will have. If it's `Left sk`, it'll be a simple
-- `BlockSignature`; if it's `Right (issuerSK, delegateSK, b)`, it will be a proxy
-- signature, and if `b :: Bool` is false, it'll be a simple proxy secret key.
-- Otherwise, it'll be a proxy secret key with epochs, whose lower and upper epoch
-- bounds will be randomly generated.
--
-- Beware that
-- * genesis blocks have no leaders, and that
-- * if an epoch is `n` slots long, every `n+1`-th block will be of the genesis kind.
recursiveHeaderGen
    :: (Arbitrary (SscPayload ssc), SscHelpersClass ssc, Integral a)
    => [Either SecretKey (SecretKey, SecretKey, Bool)]
    -> [(a, a)]
    -> [T.BlockHeader ssc]
    -> Gen [T.BlockHeader ssc]
recursiveHeaderGen (eitherOfLeader : leaders)
                   ((epoch, slot) : rest)
                   blockchain@(prevHeader : _) = do
    curHeader <- genHeader
    recursiveHeaderGen leaders rest (curHeader : blockchain)
  where
    epochCounter = fromIntegral epoch
    slotCounter = fromIntegral slot
    genHeader
      | slot == epochSlots = do
        body <- arbitrary
        return $ Left $ T.mkGenesisHeader (Just prevHeader) (epochCounter + 1) body
      | otherwise = do
        body <- arbitrary
        extraHData <- arbitrary
        lowEpoch <- choose (0, epochCounter)
        highEpoch <- choose (epochCounter, maxEpochs + 1)
        -- These two values may not be used at all. If the slot in question
        -- will have a simple signature, laziness will prevent them from
        -- being calculated. Otherwise, they'll be the proxy secret key's ω.
        let slotId = T.SlotId epochCounter slotCounter
            (leader, proxySK) =
                case eitherOfLeader of
                    Left sk -> (sk, Nothing)
                    Right (issuerSK, delegateSK, isSigEpoch) ->
                        let w = (lowEpoch, highEpoch)
                            delegatePK = toPublic delegateSK
                            curried :: Bi w => w -> ProxySecretKey w
                            curried = createProxySecretKey issuerSK delegatePK
                            proxy = if isSigEpoch
                                    then Right $ curried epochCounter
                                    else Left $ curried w
                        in (delegateSK, Just $ proxy)
        pure $ Right $
            T.mkMainHeader (Just prevHeader) slotId leader proxySK body extraHData
recursiveHeaderGen [] _ b = return b
recursiveHeaderGen _ [] b = return b
recursiveHeaderGen _ _ _  = return []


-- | This type is used to generate a blockchain, as well a list of leaders for every
-- slot with which the chain will be paired. The leaders are in reverse order to the
-- chain - the list goes from first to last leader. This is used in a `verifyHeader`
-- test.
--
-- Note that every non-empty blockchain has at least one epoch, which may be complete
-- or incomplete. To simulate this behavior, two random numbers are generated:
-- one that stands for the number of complete epochs we have, and the other for the
-- number of incomplete slots of the last epoch, which, in this instance, must exist.
--
-- A blockchain with only complete epochs is a subset of some blockchain with one
-- incomplete epoch, so if the former is desired, a simple list `takeWhile` of the list
-- this instance generates will be enough.
--
-- Note that a leader is generated for each slot.
-- (Not exactly a leader - see previous comment)
instance (Arbitrary (SscPayload ssc), SscHelpersClass ssc) =>
         Arbitrary (BlockHeaderList ssc) where
    arbitrary = BHL <$> do
        fullEpochs <- choose (startingEpoch, maxEpochs)
        incompleteEpochSize <- T.LocalSlotIndex <$> choose (1, epochSlots - 1)
        leadersList <-
            vectorOf ((epochSlots * fullEpochs) + fromIntegral incompleteEpochSize)
                arbitrary
        firstGenesisBody <- arbitrary
        let firstHeader = Left $ T.mkGenesisHeader Nothing startingEpoch firstGenesisBody
            actualLeaders = map (toPublic . either identity (view _1)) leadersList
        (, actualLeaders) <$>
            recursiveHeaderGen
                leadersList
                (range ((startingEpoch, 0), (fromIntegral fullEpochs, epochSlots)) ++
                zip (repeat $ fromIntegral (fullEpochs + 1)) [0 .. incompleteEpochSize - 1])
                -- This `range` will give us pairs with all complete epochs and for each,
                -- every slot therein.
                [firstHeader]

-- | This type is used to generate a valid blockheader and associated header
-- verification params. With regards to the block header function
-- 'Pos.Types.Blocks.Functions.verifyHeader', the blockheaders that may be part of the
-- verification parameters are guaranteed to be valid, as are the slot leaders and the
-- current slot.
newtype HeaderAndParams ssc = HAndP
    { getHAndP :: (T.VerifyHeaderParams ssc, T.BlockHeader ssc)
    } deriving (Eq, Show)

-- | A lot of the work to generate a valid sequence of blockheaders has already been done
-- in the 'Arbitrary' instance of the 'BlockHeaderList' type, so it is used here and at
-- most 3 blocks are taken from the generated list.
instance (Arbitrary (SscPayload ssc), SscHelpersClass ssc) =>
    Arbitrary (HeaderAndParams ssc) where
    arbitrary = do
        consensus <- arbitrary :: Gen Bool
        -- This integer is used as a seed to randomly choose a slot down below
        seed <- arbitrary :: Gen Int
        (headers, leaders) <- first reverse . getHeaderList <$> arbitrary
        let num = length headers
        -- 'skip' is the random number of headers that should be skipped in the header
        -- chain. This ensures different parts of it are chosen each time.
        skip <- choose (0, num - 1)
        let atMost3HeadersAndLeaders = take 3 $ drop skip headers
            (prev, header, next) =
                case atMost3HeadersAndLeaders of
                    [h] -> (Nothing, h, Nothing)
                    [h1, h2] -> (Just h1, h2, Nothing)
                    (h1 : h2 : h3 : _) -> (Just h1, h2, Just h3)
                    _ -> error "[BlockSpec] the headerchain doesn't have enough headers"
            -- This binding captures the chosen header's epoch. It is used to drop all
            -- all leaders of headers from previous epochs.
            thisEpochStartIndex = fromIntegral $ epochSlots * (header ^. T.epochIndexL)
            thisHeadersEpoch = drop thisEpochStartIndex leaders
            -- A helper function. Given integers 'x' and 'y', it chooses a random integer
            -- in the interval [x, y]
            betweenXAndY x y = fst . randomR (x, y) . mkStdGen $ seed
            betweenZeroAndN = betweenXAndY 0
            -- One of the fields in the 'VerifyHeaderParams' type is 'Just SlotId'. The
            -- following binding is where it is calculated.
            randomSlotBeforeThisHeader =
                case header of
                    -- If the header is of the genesis kind, this field is not needed.
                    Left _  -> Nothing
                    -- If it's a main blockheader, then a valid "current" SlotId for
                    -- testing is any with an epoch greater than the header's epoch and
                    -- with any slot index, or any in the same epoch but with a greater or
                    -- equal slot index than the header.
                    Right h -> -- Nothing {-
                        let (T.SlotId e s) = view T.headerSlot h
                            rndEpoch = betweenXAndY e maxBound
                            rndSlotIdx = if rndEpoch > e
                                then betweenZeroAndN (epochSlots - 1)
                                else betweenXAndY s (epochSlots - 1)
                            rndSlot = T.SlotId rndEpoch rndSlotIdx
                        in Just rndSlot
            params = T.VerifyHeaderParams
                { T.vhpVerifyConsensus = consensus
                , T.vhpPrevHeader = prev
                , T.vhpNextHeader = next
                , T.vhpCurrentSlot = randomSlotBeforeThisHeader
                , T.vhpLeaders = nonEmpty $ map T.addressHash thisHeadersEpoch
                , T.vhpMaxSize = biSize header
                }
        return . HAndP $ (params, header)
