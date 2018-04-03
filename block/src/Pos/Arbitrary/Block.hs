{-# LANGUAGE TypeOperators #-}

module Pos.Arbitrary.Block
       ( HeaderAndParams (..)
       , BlockHeaderList (..)

       , genMainBlockHeader
       , genMainBlockBody
       , genMainBlockBodyForSlot
       , genMainBlock
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))
import qualified Prelude
import           System.Random (Random, mkStdGen, randomR)
import           Test.QuickCheck (Arbitrary (..), Gen, choose, suchThat, vectorOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core (genSlotId)
import           Pos.Arbitrary.Delegation (genDlgPayload)
import           Pos.Arbitrary.Ssc (SscPayloadDependsOnSlot (..), genSscPayload,
                                    genSscPayloadForSlot)
import           Pos.Arbitrary.Txp (genTxPayload)
import           Pos.Arbitrary.Update (genUpdatePayload)
import           Pos.Binary.Class (biSize)
import qualified Pos.Block.Base as T
import qualified Pos.Block.Logic.Integrity as T
import           Pos.Block.Slog.Types (SlogUndo)
import           Pos.Block.Types (Undo (..))
import           Pos.Core (HasProtocolConstants, HasGenesisHash, HeaderHash,
                           GenesisHash (..), genesisHash, epochSlots)
import qualified Pos.Core as Core
import qualified Pos.Core.Block as T
import           Pos.Crypto (ProtocolMagic, PublicKey, SecretKey, createPsk, hash,
                             toPublic)
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Data.Attributes (areAttributesKnown)

newtype BodyDependsOnSlot b = BodyDependsOnSlot
    { genBodyDepsOnSlot :: Core.SlotId -> Gen (T.Body b)
    }

------------------------------------------------------------------------------------------
-- Arbitrary instances for Blockchain related types
------------------------------------------------------------------------------------------

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary T.BlockHeader where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) =>
    Arbitrary T.BlockSignature where
    arbitrary = genericArbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- GenesisBlockchain
------------------------------------------------------------------------------------------

instance Arbitrary T.GenesisExtraHeaderData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.GenesisExtraBodyData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.GenesisBlockHeader where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (T.BodyProof T.GenesisBlockchain) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (T.ConsensusData T.GenesisBlockchain) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (BodyDependsOnSlot T.GenesisBlockchain) where
    arbitrary = pure $ BodyDependsOnSlot $ \_ -> arbitrary

instance Arbitrary (T.Body T.GenesisBlockchain) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( HasProtocolMagic
         , HasProtocolConstants
         , HasGenesisHash
         ) =>
         Arbitrary T.GenesisBlock where
    arbitrary = T.mkGenesisBlock protocolMagic
        <$> (maybe (Left (GenesisHash genesisHash)) Right <$> arbitrary)
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- MainBlockchain
------------------------------------------------------------------------------------------

-- | Generate a 'MainBlockHeader' given a parent hash, difficulty and body.
genMainBlockHeader
    :: ProtocolMagic
    -> Core.ProtocolConstants
    -> HeaderHash
    -> Core.ChainDifficulty
    -> T.Body T.MainBlockchain
    -> Gen T.MainBlockHeader
genMainBlockHeader pm pc prevHash difficulty body =
    T.mkMainHeaderExplicit pm <$> pure prevHash
                              <*> pure difficulty
                              <*> genSlotId pc
                              <*> arbitrary -- SecretKey
                              <*> pure Nothing
                              <*> pure body
                              <*> arbitrary

instance ( HasProtocolMagic
         , HasProtocolConstants
         ) =>
         Arbitrary T.MainBlockHeader where
    arbitrary = do
        prevHash <- arbitrary
        difficulty <- arbitrary
        body <- arbitrary
        genMainBlockHeader protocolMagic Core.protocolConstants prevHash difficulty body
    shrink = genericShrink

instance Arbitrary T.MainExtraHeaderData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MainExtraBodyData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) =>
    Arbitrary (T.BodyProof T.MainBlockchain) where
    arbitrary = genericArbitrary
    shrink T.MainProof {..} =
        [T.MainProof txp mpcp prxp updp
        | (txp, mpcp, prxp, updp) <-
            shrink (mpTxProof, mpMpcProof, mpProxySKsProof, mpUpdateProof)
        ]

instance (HasProtocolConstants, HasProtocolMagic) =>
    Arbitrary (T.ConsensusData T.MainBlockchain) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary T.MainToSign where
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
    -> Gen (T.Body T.MainBlockchain)
genMainBlockBody pm epoch =
    T.MainBody <$> genTxPayload pm
               <*> genSscPayload pm
               <*> genDlgPayload pm epoch
               <*> genUpdatePayload pm

genMainBlockBodyForSlot
    :: ProtocolMagic
    -> Core.ProtocolConstants
    -> Core.SlotId
    -> Gen (T.Body T.MainBlockchain)
genMainBlockBodyForSlot pm pc slotId = do
    txpPayload <- genTxPayload pm
    sscPayload <- genSscPayloadForSlot pm pc slotId
    dlgPayload <- genDlgPayload pm (Core.siEpoch slotId)
    updPayload <- genUpdatePayload pm
    pure $ T.MainBody txpPayload sscPayload dlgPayload updPayload

instance (HasProtocolConstants, HasProtocolMagic) =>
         Arbitrary (BodyDependsOnSlot T.MainBlockchain) where
    arbitrary = pure $ BodyDependsOnSlot $ \slotId -> do
        txPayload   <- arbitrary
        generator   <- genPayloadDependsOnSlot <$> arbitrary
        mpcData     <- generator slotId
        dlgPayload  <- genDlgPayload protocolMagic $ Core.siEpoch slotId
        mpcUpload   <- arbitrary
        return $ T.MainBody txPayload mpcData dlgPayload mpcUpload


instance (HasProtocolMagic) => Arbitrary (T.Body T.MainBlockchain) where
    arbitrary = genericArbitrary
    shrink mb =
        [ T.MainBody txp sscp dlgp updp
        | (txp, sscp, dlgp, updp) <-
            shrink (mb ^. T.mbTxPayload,
                    mb ^. T.mbSscPayload,
                    mb ^. T.mbDlgPayload,
                    mb ^. T.mbUpdatePayload)
        ]

-- | Generate a main block (slot is chosen arbitrarily).
-- You choose the previous header hash.
genMainBlock
    :: ProtocolMagic
    -> Core.ProtocolConstants
    -> HeaderHash
    -> Core.ChainDifficulty
    -> Gen T.MainBlock
genMainBlock pm pc prevHash difficulty = do
    slot <- genSlotId pc
    body <- genMainBlockBodyForSlot pm pc slot
    extraBodyData <- arbitrary
    extraHeaderData <- T.MainExtraHeaderData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure (hash extraBodyData)
    header <- T.mkMainHeaderExplicit pm prevHash difficulty slot
        <$> arbitrary
        <*> pure Nothing
        <*> pure body
        <*> pure extraHeaderData
    pure $ T.UnsafeGenericBlock header body extraBodyData

instance ( HasProtocolConstants
         , HasProtocolMagic
         , HasGenesisHash
         ) =>
         Arbitrary T.MainBlock where
    arbitrary = do
        slot <- arbitrary
        BodyDependsOnSlot {..} <- arbitrary
        body <- genBodyDepsOnSlot slot
        extraBodyData <- arbitrary
        extraHeaderData <- T.MainExtraHeaderData
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> pure (hash extraBodyData)
        header <-
            T.mkMainHeader protocolMagic
                <$> (maybe (Left (GenesisHash genesisHash)) Right <$> arbitrary)
                <*> pure slot
                <*> arbitrary
                <*> pure Nothing
                <*> pure body
                <*> pure extraHeaderData
        return $ T.UnsafeGenericBlock header body extraBodyData
    shrink = genericShrink

instance Buildable (T.BlockHeader, PublicKey) where
    build (block, key) =
        bprint
            ( build%"\n"%
              build%"\n"
            ) block key

newtype BlockHeaderList = BHL
    { getHeaderList :: ([T.BlockHeader], [PublicKey])
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
    :: ( HasProtocolConstants   -- Can't remove this unfortunately....
                                -- We first have to make generators for
                                -- other things which are parameterized on
                                -- the constants and magic etc. so we can use
                                -- them in here.
       , HasProtocolMagic
       )
    => GenesisHash
    -> Bool -- ^ Whether to create genesis block before creating main block for 0th slot
    -> [Either SecretKey (SecretKey, SecretKey)]
    -> [Core.SlotId]
    -> [T.BlockHeader]
    -> Gen [T.BlockHeader]
recursiveHeaderGen gHash
                   genesis
                   (eitherOfLeader : leaders)
                   (Core.SlotId{..} : rest)
                   blockchain
    | genesis && Core.getSlotIndex siSlot == 0 = do
          gBody <- arbitrary
          let pHeader = maybe (Left gHash) Right ((fmap fst . uncons) blockchain)
              gHeader = T.BlockHeaderGenesis $ T.mkGenesisHeader protocolMagic pHeader siEpoch gBody
          mHeader <- genMainHeader (Just gHeader)
          recursiveHeaderGen gHash True leaders rest (mHeader : gHeader : blockchain)
    | otherwise = do
          curHeader <- genMainHeader ((fmap fst . uncons) blockchain)
          recursiveHeaderGen gHash True leaders rest (curHeader : blockchain)
  where
    genMainHeader prevHeader = do
        body <- arbitrary
        extraHData <- arbitrary
        -- These two values may not be used at all. If the slot in question
        -- will have a simple signature, laziness will prevent them from
        -- being calculated. Otherwise, they'll be the proxy secret key's ω.
        let slotId = Core.SlotId siEpoch siSlot
            (leader, proxySK) = case eitherOfLeader of
                Left sk -> (sk, Nothing)
                Right (issuerSK, delegateSK) ->
                    let delegatePK = toPublic delegateSK
                        proxy = ( createPsk protocolMagic issuerSK delegatePK (Core.HeavyDlgIndex siEpoch)
                                , toPublic issuerSK)
                    in (delegateSK, Just proxy)
        pure $ T.BlockHeaderMain $
            T.mkMainHeader protocolMagic (maybe (Left gHash) Right prevHeader) slotId leader proxySK body extraHData
recursiveHeaderGen _ _ [] _ b = return b
recursiveHeaderGen _ _ _ [] b = return b


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
instance ( HasProtocolMagic
         , HasProtocolConstants
         , HasGenesisHash
         ) =>
         Arbitrary BlockHeaderList where
    arbitrary = do
        incompleteEpochSize <- choose (1, epochSlots - 1)
        let slot = Core.SlotId 0 minBound
        generateBHL (GenesisHash genesisHash) True slot (epochSlots * bhlEpochs + incompleteEpochSize)

generateBHL
    :: ( HasProtocolMagic      -- Can't remove these yet.
       , HasProtocolConstants  -- See comment in recursiveHeaderGen
       )
    => GenesisHash
    -> Bool         -- ^ Whether to create genesis block before creating main
                    --    block for 0th slot
    -> Core.SlotId     -- ^ Start slot
    -> Core.SlotCount  -- ^ Slot count
    -> Gen BlockHeaderList
generateBHL gHash createInitGenesis startSlot slotCount = BHL <$> do
    let correctLeaderGen :: Gen (Either SecretKey (SecretKey, SecretKey))
        correctLeaderGen =
            -- We don't want to create blocks with self-signed psks
            let issDelDiff (Left _)      = True
                issDelDiff (Right (i,d)) = i /= d
            in arbitrary `suchThat` issDelDiff
    leadersList <- vectorOf (fromIntegral slotCount) correctLeaderGen
    let actualLeaders = map (toPublic . either identity (view _1)) leadersList
        slotIdsRange =
            take (fromIntegral slotCount) $
            map Core.unflattenSlotId
                [Core.flattenSlotId startSlot ..]
    (, actualLeaders) <$>
        recursiveHeaderGen
            gHash
            createInitGenesis
            leadersList
            slotIdsRange
            []

-- | This type is used to generate a valid blockheader and associated header
-- verification params. With regards to the block header function
-- 'Pos.Types.Blocks.Functions.verifyHeader', the blockheaders that may be
-- part of the verification parameters are guaranteed to be valid, as are the
-- slot leaders and the current slot.
newtype HeaderAndParams = HAndP
    { getHAndP :: (T.VerifyHeaderParams, T.BlockHeader)
    } deriving (Eq, Show)

-- | A lot of the work to generate a valid sequence of blockheaders has
-- already been done in the 'Arbitrary' instance of the 'BlockHeaderList'
-- type, so it is used here and at most 3 blocks are taken from the generated
-- list.
instance (HasProtocolConstants, HasProtocolMagic, HasGenesisHash) =>
    Arbitrary HeaderAndParams where
    arbitrary = do
        -- This integer is used as a seed to randomly choose a slot down below
        seed <- arbitrary :: Gen Int
        startSlot <- Core.SlotId <$> choose (0, bhlMaxStartingEpoch) <*> arbitrary
        (headers, leaders) <- first reverse . getHeaderList <$>
            (generateBHL (GenesisHash genesisHash) True startSlot =<< choose (1, 2))
        let num = length headers
        -- 'skip' is the random number of headers that should be skipped in
        -- the header chain. This ensures different parts of it are chosen
        -- each time.
        skip <- choose (0, num - 1)
        let atMost2HeadersAndLeaders = take 2 $ drop skip headers
            (prev, header) =
                case atMost2HeadersAndLeaders of
                    [h]      -> (Nothing, h)
                    [h1, h2] -> (Just h1, h2)
                    _        -> error "[BlockSpec] the headerchain doesn't have enough headers"
            -- This binding captures the chosen header's epoch. It is used to
            -- drop all all leaders of headers from previous epochs.
            thisEpochStartIndex = fromIntegral epochSlots *
                                  fromIntegral (header ^. Core.epochIndexL)
            thisHeadersEpoch = drop thisEpochStartIndex leaders
            -- A helper function. Given integers 'x' and 'y', it chooses a
            -- random integer in the interval [x, y]
            betweenXAndY :: Random a => a -> a -> a
            betweenXAndY x y = fst . randomR (x, y) . mkStdGen $ seed
            -- One of the fields in the 'VerifyHeaderParams' type is 'Just
            -- SlotId'. The following binding is where it is calculated.
            randomSlotBeforeThisHeader =
                case header of
                    -- If the header is of the genesis kind, this field is
                    -- not needed.
                    T.BlockHeaderGenesis _  -> Nothing
                    -- If it's a main blockheader, then a valid "current"
                    -- SlotId for testing is any with an epoch greater than
                    -- the header's epoch and with any slot index, or any in
                    -- the same epoch but with a greater or equal slot index
                    -- than the header.
                    T.BlockHeaderMain h -> -- Nothing {-
                        let (Core.SlotId e s) = view Core.headerSlotL h
                            rndEpoch :: Core.EpochIndex
                            rndEpoch = betweenXAndY e maxBound
                            rndSlotIdx :: Core.LocalSlotIndex
                            rndSlotIdx = if rndEpoch > e
                                then betweenXAndY minBound maxBound
                                else betweenXAndY s maxBound
                            rndSlot = Core.SlotId rndEpoch rndSlotIdx
                        in Just rndSlot
            hasUnknownAttributes =
                not . areAttributesKnown $
                case header of
                    T.BlockHeaderGenesis h -> h ^. Core.gbhExtra . T.gehAttributes
                    T.BlockHeaderMain h    -> h ^. Core.gbhExtra . T.mehAttributes
            params = T.VerifyHeaderParams
                { T.vhpPrevHeader = prev
                , T.vhpCurrentSlot = randomSlotBeforeThisHeader
                , T.vhpLeaders = nonEmpty $ map Core.addressHash thisHeadersEpoch
                , T.vhpMaxSize = Just (biSize header)
                , T.vhpVerifyNoUnknown = not hasUnknownAttributes
                }
        return . HAndP $ (params, header)

------------------------------------------------------------------------
-- Pos.Block.Slog.Types
------------------------------------------------------------------------

instance Arbitrary SlogUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasProtocolMagic) => Arbitrary Undo where
    arbitrary = genericArbitrary
    shrink = genericShrink
