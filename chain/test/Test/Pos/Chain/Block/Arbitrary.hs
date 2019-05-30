{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Chain.Block.Arbitrary
       ( HeaderAndParams (..)
       , BlockHeaderList (..)

       , genMainBlockHeader
       , genMainBlockBody
       , genMainBlockBodyForSlot
       , genMainBlock
       , genHeaderAndParams
       , genStubbedBHL
       ) where

import qualified Prelude
import           Universum

import qualified Data.List as List
import qualified Data.Set as Set
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable
import           System.Random (Random, mkStdGen, randomR)
import           Test.QuickCheck (Arbitrary (..), Gen, choose, suchThat)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Binary.Class (biSize)
import           Pos.Chain.Block (ConsensusEraLeaders (..), HeaderHash,
                     headerLastSlotInfo, mkMainBlock, mkMainBlockExplicit)
import qualified Pos.Chain.Block as Block
import qualified Pos.Chain.Block.Slog.LastBlkSlots as LastBlkSlots
import qualified Pos.Chain.Delegation as Core
import           Pos.Chain.Genesis (GenesisHash (..))
import           Pos.Chain.Update (ConsensusEra (..),
                     ObftConsensusStrictness (..))
import           Pos.Core (BlockCount (..), EpochOrSlot (..), SlotId (..),
                     getEpochOrSlot, localSlotIndexMaxBound,
                     localSlotIndexMinBound)
import qualified Pos.Core as Core
import           Pos.Core.Attributes (areAttributesKnown)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Slotting (LocalSlotIndex (..), SlotCount (..),
                     epochOrSlotToSlot)
import           Pos.Crypto (ProtocolMagic, PublicKey, SecretKey, createPsk,
                     toPublic)

import           Test.Pos.Chain.Delegation.Arbitrary (genDlgPayload)
import           Test.Pos.Chain.Genesis.Dummy (dummyEpochSlots,
                     dummyGenesisHash)
import           Test.Pos.Chain.Ssc.Arbitrary (SscPayloadDependsOnSlot (..),
                     genSscPayload, genSscPayloadForSlot)
import           Test.Pos.Chain.Txp.Arbitrary (genTxPayload)
import           Test.Pos.Chain.Update.Arbitrary (genUpdatePayload)
import           Test.Pos.Core.Arbitrary (genSlotId)

newtype BodyDependsOnSlot body = BodyDependsOnSlot
    { genBodyDepsOnSlot :: SlotId -> Gen body
    }

------------------------------------------------------------------------------------------
-- Arbitrary instances for Blockchain related types
------------------------------------------------------------------------------------------

instance Arbitrary Block.BlockHeader where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.BlockSignature where
    arbitrary = genericArbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- GenesisBlockchain
------------------------------------------------------------------------------------------

instance Arbitrary Block.GenesisExtraHeaderData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.GenesisExtraBodyData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.GenesisBlockHeader where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.GenesisProof where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.GenesisConsensusData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (BodyDependsOnSlot Block.GenesisBody) where
    arbitrary = pure $ BodyDependsOnSlot $ \_ -> arbitrary

instance Arbitrary Block.GenesisBody where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.GenesisBlock where
    arbitrary = Block.mkGenesisBlock
        <$> arbitrary
        <*> (maybe (Left dummyGenesisHash) Right <$> arbitrary)
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- MainBlockchain
------------------------------------------------------------------------------------------

-- | Generate a 'MainBlockHeader' given a parent hash, difficulty and body.
genMainBlockHeader
    :: ProtocolMagic
    -> HeaderHash
    -> Core.ChainDifficulty
    -> Block.MainBody
    -> Gen Block.MainBlockHeader
genMainBlockHeader pm prevHash difficulty body =
    Block.mkMainHeaderExplicit pm <$> pure prevHash
                              <*> pure difficulty
                              <*> genSlotId dummyEpochSlots
                              <*> arbitrary -- SecretKey
                              <*> pure Nothing
                              <*> pure body
                              <*> arbitrary

instance Arbitrary Block.MainBlockHeader where
    arbitrary = do
        prevHash <- arbitrary
        difficulty <- arbitrary
        body <- arbitrary
        pm <- arbitrary
        genMainBlockHeader pm prevHash difficulty body
    shrink = genericShrink

instance Arbitrary Block.MainExtraHeaderData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.MainExtraBodyData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.MainProof where
    arbitrary = genericArbitrary
    shrink Block.MainProof {..} =
        [Block.MainProof txp mpcp prxp updp
        | (txp, mpcp, prxp, updp) <-
            shrink (mpTxProof, mpMpcProof, mpProxySKsProof, mpUpdateProof)
        ]

instance Arbitrary Block.MainConsensusData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Block.MainToSign where
    arbitrary = genericArbitrary
    shrink = genericShrink

-- | In the main blockchain's body, the number of transactions must be the
-- same as the number of transaction witnesses.
--
-- Furthermore, for every transaction in index i of the list, the length of
-- its output list must be the same as the length of the i-th item in the
-- TxDistribution list.
--
-- Because of this, the Arbitrary instance for @Body
-- MainBlockchain@ ensures that for every transaction generated, a
-- transaction witness is generated as well, and the lengths of its list of
-- outputs must also be the same as the length of its corresponding
-- TxDistribution item.

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

genMainBlockBody
    :: ProtocolMagic
    -> Core.EpochIndex -- ^ For the delegation payload.
    -> Gen Block.MainBody
genMainBlockBody pm epoch =
    Block.MainBody <$> genTxPayload pm
               <*> genSscPayload pm
               <*> genDlgPayload pm epoch
               <*> genUpdatePayload pm

genMainBlockBodyForSlot
    :: ProtocolMagic
    -> SlotId
    -> Gen Block.MainBody
genMainBlockBodyForSlot pm slotId = do
    txpPayload <- genTxPayload pm
    sscPayload <- genSscPayloadForSlot pm slotId
    dlgPayload <- genDlgPayload pm (Core.siEpoch slotId)
    updPayload <- genUpdatePayload pm
    pure $ Block.MainBody txpPayload sscPayload dlgPayload updPayload

instance Arbitrary (BodyDependsOnSlot Block.MainBody) where
    arbitrary = pure $ BodyDependsOnSlot $ \slotId -> do
        txPayload   <- arbitrary
        generator   <- genPayloadDependsOnSlot <$> arbitrary
        mpcData     <- generator slotId
        pm          <- arbitrary
        dlgPayload  <- genDlgPayload pm $ Core.siEpoch slotId
        mpcUpload   <- arbitrary
        return $ Block.MainBody txPayload mpcData dlgPayload mpcUpload

instance Arbitrary Block.MainBody where
    arbitrary = genericArbitrary
    shrink mb =
        [ Block.MainBody txp sscp dlgp updp
        | (txp, sscp, dlgp, updp) <-
            shrink (mb ^. Block.mbTxPayload,
                    mb ^. Block.mbSscPayload,
                    mb ^. Block.mbDlgPayload,
                    mb ^. Block.mbUpdatePayload)
        ]

-- | Generate a main block (slot is chosen arbitrarily).
-- You choose the previous header hash.
genMainBlock
    :: ProtocolMagic
    -> HeaderHash
    -> Core.ChainDifficulty
    -> Gen Block.MainBlock
genMainBlock pm prevHash difficulty = do
    bv <- arbitrary
    sv <- arbitrary
    slot <- genSlotId dummyEpochSlots
    sk <- arbitrary
    body <- genMainBlockBodyForSlot pm slot
    pure $ mkMainBlockExplicit pm bv sv prevHash difficulty slot sk Nothing body

instance Arbitrary Block.MainBlock where
    arbitrary = do
        slot <- arbitrary
        pm <- arbitrary
        bv <- arbitrary
        sv <- arbitrary
        prevHeader <- maybe (Left dummyGenesisHash) Right <$> arbitrary
        sk <- arbitrary
        BodyDependsOnSlot {..} <- arbitrary :: Gen (BodyDependsOnSlot Block.MainBody)
        body <- genBodyDepsOnSlot slot
        pure $ mkMainBlock pm bv sv prevHeader slot sk Nothing body
    shrink = genericShrink

instance Buildable (Block.BlockHeader, PublicKey) where
    build (block, key) =
        bprint
            ( build%"\n"%
              build%"\n"
            ) block key

newtype BlockHeaderList = BHL
    { getHeaderList :: ([Block.BlockHeader], [PublicKey])
    } deriving (Eq)

instance Show BlockHeaderList where
    show = toString . unlines . map pretty . uncurry zip . getHeaderList

-- | Generation of arbitrary, valid headerchain along with a list of leaders
-- for each epoch.
--
-- Because 'verifyHeaders' assumes the head of the list is the most recent
-- block, this function is tail-recursive: while keeping track of the current
-- block and epoch/slot, it adds the most recent one to the head of the
-- header list it'll return when done.
--
-- The @[Either SecretKey (SecretKey, SecretKey, Bool)]@ type is for
-- determining what kind of signature the slot's block will have. If it's
-- @Left sk@, it'll be a simple 'BlockSignature'; if it's @Right (issuerSK,
-- delegateSK, b)@, it will be a proxy signature, and if @b :: Bool@ is
-- false, it'll be a simple proxy secret key. Otherwise, it'll be a proxy
-- secret key with epochs, whose lower and upper epoch bounds will be
-- randomly generated.
--
-- Beware that
--   * genesis blocks have no leaders, and that
--   * if an epoch is `n` slots long, every `n+1`-th block will be of the
--     genesis kind.
recursiveHeaderGen
    :: ProtocolMagic
    -> ConsensusEra
    -> GenesisHash
    -> Bool -- ^ Whether to create genesis block before creating main block for 0th slot
    -> [Either SecretKey (SecretKey, SecretKey)]
    -> [SlotId]
    -> [Block.BlockHeader]
    -> Gen [Block.BlockHeader]
recursiveHeaderGen pm
                   era
                   gHash
                   genesis
                   (eitherOfLeader : leaders)
                   (SlotId{..} : rest)
                   blockchain
    | genesis && era == Original && Core.getSlotIndex siSlot == 0 = do
          gBody <- arbitrary
          let pHeader = maybe (Left gHash) Right ((fmap fst . uncons) blockchain)
              gHeader = Block.BlockHeaderGenesis $ Block.mkGenesisHeader pm pHeader siEpoch gBody
          mHeader <- genMainHeader (Just gHeader)
          recursiveHeaderGen pm era gHash True leaders rest (mHeader : gHeader : blockchain)
    | otherwise = do
          curHeader <- genMainHeader ((fmap fst . uncons) blockchain)
          recursiveHeaderGen pm era gHash True leaders rest (curHeader : blockchain)
  where
    genMainHeader prevHeader = do
        body <- arbitrary
        extraHData <- arbitrary
        -- These two values may not be used at all. If the slot in question
        -- will have a simple signature, laziness will prevent them from
        -- being calculated. Otherwise, they'll be the proxy secret key's ω.
        let slotId = SlotId siEpoch siSlot
            (leader, proxySK) = case eitherOfLeader of
                Left sk -> (sk, Nothing)
                Right (issuerSK, delegateSK) ->
                    let delegatePK = toPublic delegateSK
                        proxy = ( createPsk pm issuerSK delegatePK (Core.HeavyDlgIndex siEpoch)
                                , toPublic issuerSK)
                    in (delegateSK, Just proxy)
        pure $ Block.BlockHeaderMain $
            Block.mkMainHeader pm (maybe (Left gHash) Right prevHeader) slotId leader proxySK body extraHData
recursiveHeaderGen _ _ _ _ [] _ b = return b
recursiveHeaderGen _ _ _ _ _ [] b = return b


-- | Maximum start epoch in block header verification tests
bhlMaxStartingEpoch :: Integral a => a
bhlMaxStartingEpoch = 1000000

-- | Amount of full epochs in block header verification tests
bhlEpochs :: Integral a => a
bhlEpochs = 2

-- | This type is used to generate a blockchain, as well a list of leaders
-- for every slot with which the chain will be paired. The leaders are in
-- reverse order to the chain - the list goes from first to last leader. This
-- is used in a `verifyHeader` test.
--
-- Note that every non-empty blockchain has at least one epoch, which may be
-- complete or incomplete. To simulate this behavior, two random numbers are
-- generated: one that stands for the number of complete epochs we have, and
-- the other for the number of incomplete slots of the last epoch, which, in
-- this instance, must exist.
--
-- A blockchain with only complete epochs is a subset of some blockchain with
-- one incomplete epoch, so if the former is desired, a simple list
-- `takeWhile` of the list this instance generates will be enough.
--
-- Note that a leader is generated for each slot.
-- (Not exactly a leader - see previous comment)
instance Arbitrary BlockHeaderList where
    arbitrary = do
        pm <- arbitrary
        era <- arbitrary
        genStubbedBHL pm era

genStubbedBHL
    :: ProtocolMagic
    -> ConsensusEra
    -> Gen BlockHeaderList
genStubbedBHL pm era = do
    incompleteEpochSize <- choose (1, dummyEpochSlots - 1)
    let slot = SlotId 0 localSlotIndexMinBound
    generateBHL pm era dummyGenesisHash True slot (dummyEpochSlots * bhlEpochs + incompleteEpochSize)

generateBHL
    :: ProtocolMagic
    -> ConsensusEra
    -> GenesisHash
    -> Bool         -- ^ Whether to create genesis block before creating main
                    --    block for 0th slot
    -> SlotId     -- ^ Start slot
    -> SlotCount  -- ^ Slot count
    -> Gen BlockHeaderList
generateBHL pm era gHash createInitGenesis startSlot slotCount = BHL <$> do
    leadersList <- genLeaderKeyList $ fromIntegral slotCount
    let actualLeaders = map (toPublic . either identity (view _1)) leadersList
        slotIdsRange =
            take (fromIntegral slotCount) $
            map (Core.unflattenSlotId dummyEpochSlots)
                [Core.flattenSlotId dummyEpochSlots startSlot ..]
    (, actualLeaders) <$>
        recursiveHeaderGen
            pm
            era
            gHash
            createInitGenesis
            leadersList
            slotIdsRange
            []

-- Generate a unique list of leader keys of the specified length. Needs to be
-- unique so that block vaidation doesn't fail when validating ObftLenient.
genLeaderKeyList :: Int -> Gen [Either SecretKey (SecretKey, SecretKey)]
genLeaderKeyList count =
    loop 0 []
  where
    loop :: Int -> [Either SecretKey (SecretKey, SecretKey)] -> Gen [Either SecretKey (SecretKey, SecretKey)]
    loop n !acc
        | n >= count = pure acc
        | otherwise = do
            key <- correctLeaderGen
            -- New keys that are already present in the list are discarded.
            if key `elem` acc
                then loop n acc
                else loop (n + 1) (key : acc)

    correctLeaderGen :: Gen (Either SecretKey (SecretKey, SecretKey))
    correctLeaderGen =
        -- We don't want to create blocks with self-signed psks
        let issDelDiff (Left _)      = True
            issDelDiff (Right (i,d)) = i /= d
        in arbitrary `suchThat` issDelDiff

-- | This type is used to generate a valid blockheader and associated header
-- verification params. With regards to the block header function
-- 'Pos.Types.Blocks.Functions.verifyHeader', the blockheaders that may be
-- part of the verification parameters are guaranteed to be valid, as are the
-- slot leaders and the current slot.
data HeaderAndParams = HeaderAndParams
    { hapHeader :: Block.BlockHeader
    , hapParams :: Block.VerifyHeaderParams
    } deriving (Eq, Show)

-- This generator produces a header and a set of params for testing that header.
genHeaderAndParams :: ProtocolMagic -> ConsensusEra -> Gen HeaderAndParams
genHeaderAndParams pm era = do
    -- This Int is used as a seed to randomly choose a slot down below
    seed <- arbitrary :: Gen Int
    -- If the blkSecurityParam is too low (ie < 10) then ObftLenient is likely
    -- to fail.
    blkSecurityParam <- BlockCount <$> choose (10, 50)
    slotsPerEpoch <- SlotCount . (getBlockCount blkSecurityParam *) <$> choose (2, 10)
    startSlot <- SlotId <$> choose (0, bhlMaxStartingEpoch)
                    <*> (UnsafeLocalSlotIndex <$> choose (0, fromIntegral (getSlotCount slotsPerEpoch) - 1))
    -- Create up to 10 slots, and trim them later.
    slotCount <- choose (2, 10)
    headers <- reverse . fst . getHeaderList
                    <$> generateBHL pm era dummyGenesisHash True startSlot slotCount

    -- 'skip' is the random number of headers that should be skipped in
    -- the header chain. This ensures different parts of it are chosen
    -- each time.
    skip <- choose (0, length headers - 2)
    let (prev, header) =
            case take 2 $ drop skip headers of
                [h]      -> (Nothing, h)
                [h1, h2] -> (Just h1, h2)
                []       -> error "[BlockSpec] empty headerchain"
                _        -> error "[BlockSpec] the headerchain doesn't have enough headers"
        -- A helper function. Given integers 'x' and 'y', it chooses a
        -- random integer in the interval [x, y]
        betweenXAndY :: Random a => a -> a -> a
        betweenXAndY x y = fst . randomR (x, y) $ mkStdGen seed
        -- One of the fields in the 'VerifyHeaderParams' type is 'Just
        -- SlotId'. The following binding is where it is calculated.
        randomSlotBeforeThisHeader =
            case header of
                -- If the header is of the genesis kind, this field is
                -- not needed.
                Block.BlockHeaderGenesis _  -> Nothing
                -- If it's a main blockheader, then a valid "current"
                -- SlotId for testing is any with an epoch greater than
                -- the header's epoch and with any slot index, or any in
                -- the same epoch but with a greater or equal slot index
                -- than the header.
                Block.BlockHeaderMain h ->
                    let (SlotId e s) = view Block.headerSlotL h
                        rndEpoch :: Core.EpochIndex
                        rndEpoch = betweenXAndY e maxBound
                        rndSlotIdx :: LocalSlotIndex
                        rndSlotIdx = if rndEpoch > e
                            then betweenXAndY localSlotIndexMinBound (localSlotIndexMaxBound slotsPerEpoch)
                            else betweenXAndY s (localSlotIndexMaxBound slotsPerEpoch)
                        rndSlot = SlotId rndEpoch rndSlotIdx
                    in Just rndSlot
        hasUnknownAttributes =
            not . areAttributesKnown $
            case header of
                Block.BlockHeaderGenesis h -> h ^. Block.gbhExtra . Block.gehAttributes
                Block.BlockHeaderMain h    -> h ^. Block.gbhExtra . Block.mehAttributes

        thisEpochLeaderSchedule :: Maybe (NonEmpty (Core.AddressHash PublicKey))
        thisEpochLeaderSchedule =
            mkEpochLeaderSchedule era (getEpochOrSlot header) headers

        params = Block.VerifyHeaderParams
            { Block.vhpPrevHeader = prev
            , Block.vhpCurrentSlot = randomSlotBeforeThisHeader
            , Block.vhpLeaders = case era of
                Original         -> OriginalLeaders <$> thisEpochLeaderSchedule
                OBFT ObftStrict  -> ObftStrictLeaders <$> thisEpochLeaderSchedule
                OBFT ObftLenient ->
                    pure $ ObftLenientLeaders
                            (Set.fromList $ mapMaybe (fmap Core.addressHash . Block.headerLeaderKey) headers)
                            blkSecurityParam
                            (LastBlkSlots.updateMany (LastBlkSlots.create (fromIntegral $ getBlockCount blkSecurityParam)) . OldestFirst $ mapMaybe (headerLastSlotInfo slotsPerEpoch) headers)
            , Block.vhpMaxSize = Just (biSize header)
            , Block.vhpVerifyNoUnknown = not hasUnknownAttributes
            , Block.vhpConsensusEra = era
            }
    return $ HeaderAndParams header params

-- Pad the head of a list of block headers to generate a list that is long enough
-- to index correctly during validation. Use the EpochOrSlot of the target
-- BlockHeader to and the header index to calculate the number of fake leader
-- keys to prepend to the header
mkEpochLeaderSchedule :: ConsensusEra -> EpochOrSlot -> [Block.BlockHeader] -> Maybe (NonEmpty (Core.AddressHash PublicKey))
mkEpochLeaderSchedule era eos hdrs =
    case List.elemIndex eos (map getEpochOrSlot hdrs) of
        Nothing -> Nothing
        Just idx ->
            let count = prependCount idx in
            nonEmpty .
                (if count >= 0
                    then (replicate count fakeLeaderKey ++)
                    else List.drop (- count - extra)
                    )
                $ mapMaybe (fmap Core.addressHash . Block.headerLeaderKey) hdrs
  where
    fakeLeaderKey :: Core.AddressHash PublicKey
    fakeLeaderKey = Core.unsafeAddressHash ("fake leader key" :: ByteString)

    prependCount :: Int -> Int
    prependCount idx =
        fromIntegral (getSlotIndex . siSlot $ epochOrSlotToSlot eos) - idx

    -- Need this because in the validation code, the indexing for the slot
    -- leader schedule starts at 1 for the Original chain (due to the epoch
    -- boundary block) but at 0 for ObftStrict.
    extra :: Int
    extra =
        case era of
            Original -> 1
            OBFT ObftStrict -> 0
            OBFT ObftLenient ->
                -- This should never happen.
                Prelude.error "Test.Pos.Chain.Block.Arbitrary.mkEpochLeaderSchedule ObftLenient"


-- | A lot of the work to generate a valid sequence of blockheaders has
-- already been done in the 'Arbitrary' instance of the 'BlockHeaderList'
-- type, so it is used here and at most 3 blocks are taken from the generated
-- list.
instance Arbitrary HeaderAndParams where
    arbitrary = do
        pm <- arbitrary
        era <- arbitrary
        genHeaderAndParams pm era
