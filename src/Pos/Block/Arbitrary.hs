{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Block.Arbitrary
       ( HeaderAndParams (..)
       , BlockHeaderList (..)
       ) where

import           Universum

import           Control.Lens                      (to)
import qualified Data.Text.Buildable               as Buildable
import           Formatting                        (bprint, build, (%))
import qualified Prelude
import           System.Random                     (mkStdGen, randomR)
import           Test.QuickCheck                   (Arbitrary (..), Gen, choose, suchThat,
                                                    vectorOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Binary.Class                  (Bi, Raw, biSize)
import qualified Pos.Block.Core                    as T
import           Pos.Block.Network                 as T
import qualified Pos.Block.Pure                    as T
import           Pos.Constants                     (epochSlots)
import qualified Pos.Core                          as Core
import           Pos.Crypto                        (ProxySecretKey, PublicKey, SecretKey,
                                                    createPsk, hash, toPublic)
import           Pos.Data.Attributes               (Attributes (..))
import           Pos.Delegation.Arbitrary          (genDlgPayload)
import           Pos.Ssc.Arbitrary                 (SscPayloadDependsOnSlot (..))
import           Pos.Ssc.Class                     (Ssc (..), SscHelpersClass)
import           Pos.Txp.Arbitrary                 ()
import qualified Pos.Types                         as T
import           Pos.Update.Arbitrary              ()
import           Pos.Util.Arbitrary                (makeSmall)
import           Pos.Util.Util                     (leftToPanic)

newtype BodyDependsOnSlot b = BodyDependsOnSlot
    { genBodyDepsOnSlot :: Core.SlotId -> Gen (T.Body b)
    }

------------------------------------------------------------------------------------------
-- Arbitrary instances for Blockchain related types
------------------------------------------------------------------------------------------

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.BlockSignature ssc) where
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

instance Arbitrary (T.GenesisBlockHeader ssc) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (T.BodyProof (T.GenesisBlockchain ssc)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (T.ConsensusData (T.GenesisBlockchain ssc)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (BodyDependsOnSlot (T.GenesisBlockchain ssc)) where
    arbitrary = pure $ BodyDependsOnSlot $ \_ -> arbitrary

instance Arbitrary (T.Body (T.GenesisBlockchain ssc)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary $ SscProof ssc
         , Arbitrary $ SscPayload ssc
         , SscHelpersClass ssc
         ) =>
         Arbitrary (T.GenericBlock (T.GenesisBlockchain ssc)) where
    arbitrary = T.mkGenesisBlock <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- MainBlockchain
------------------------------------------------------------------------------------------

instance ( Arbitrary (SscPayload ssc)
         , Arbitrary (SscProof ssc)
         , Bi Raw
         , SscHelpersClass ssc
         ) =>
         Arbitrary (T.MainBlockHeader ssc) where
    arbitrary =
        T.mkMainHeader <$> arbitrary <*> arbitrary <*> arbitrary <*>
        -- TODO: do not hardcode Nothing
        pure Nothing <*>
        arbitrary <*>
        arbitrary
    shrink = genericShrink

instance Arbitrary T.MainExtraHeaderData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MainExtraBodyData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arbitrary (SscProof ssc), Bi Raw) =>
    Arbitrary (T.BodyProof (T.MainBlockchain ssc)) where
    arbitrary = genericArbitrary
    shrink T.MainProof {..} =
        [T.MainProof txp mpcp prxp updp
        | (txp, mpcp, prxp, updp) <-
            shrink (mpTxProof, mpMpcProof, mpProxySKsProof, mpUpdateProof)
        ]

instance (Arbitrary (SscProof ssc), Bi Raw, Ssc ssc) =>
    Arbitrary (T.ConsensusData (T.MainBlockchain ssc)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Ssc ssc, Arbitrary (SscProof ssc)) => Arbitrary (T.MainToSign ssc) where
    arbitrary = genericArbitrary
    shrink = genericShrink

-- | In the main blockchain's body, the number of transactions must be the
-- same as the number of transaction witnesses.
--
-- Furthermore, for every transaction in index i of the list, the length of
-- its output list must be the same as the length of the i-th item in the
-- TxDistribution list.
--
-- Because of this, the Arbitrary instance for Ssc ssc => Body
-- (MainBlockchain ssc) ensures that for every transaction generated, a
-- transaction witness is generated as well, and the lengths of its list of
-- outputs must also be the same as the length of its corresponding
-- TxDistribution item.

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

instance Arbitrary (SscPayloadDependsOnSlot ssc) =>
         Arbitrary (BodyDependsOnSlot (T.MainBlockchain ssc)) where
    arbitrary = pure $ BodyDependsOnSlot $ \slotId -> makeSmall $ do
        txPayload   <- arbitrary
        generator   <- genPayloadDependsOnSlot @ssc <$> arbitrary
        mpcData     <- generator slotId
        dlgPayload  <- genDlgPayload $ Core.siEpoch slotId
        mpcUpload   <- arbitrary
        return $ T.MainBody txPayload mpcData dlgPayload mpcUpload

instance Arbitrary (SscPayload ssc) => Arbitrary (T.Body (T.MainBlockchain ssc)) where
    arbitrary = makeSmall genericArbitrary
    shrink mb =
        [ T.MainBody txp sscp dlgp updp
        | (txp, sscp, dlgp, updp) <-
            shrink (mb ^. T.mbTxPayload,
                    mb ^. T.mbSscPayload,
                    mb ^. T.mbDlgPayload,
                    mb ^. T.mbUpdatePayload)
        ]

instance ( Arbitrary $ SscPayload ssc
         , Arbitrary $ SscProof ssc
         , Arbitrary $ SscPayloadDependsOnSlot ssc
         , SscHelpersClass ssc
         ) =>
         Arbitrary (T.GenericBlock (T.MainBlockchain ssc)) where
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
            T.mkMainHeader <$> arbitrary <*> pure slot <*> arbitrary <*>
            pure Nothing <*>
            pure body <*>
            pure extraHeaderData
        return $ leftToPanic "arbitrary @MainBlock: " $
            T.recreateGenericBlock header body extraBodyData
    shrink = genericShrink

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgGetBlocks where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arbitrary (SscPayload ssc), Arbitrary (SscProof ssc), Bi Raw, SscHelpersClass ssc) =>
         Arbitrary (T.MsgHeaders ssc) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary $ SscPayload ssc
         , Arbitrary (SscProof ssc)
         , Arbitrary (SscPayloadDependsOnSlot ssc)
         , SscHelpersClass ssc
         ) =>
         Arbitrary (T.MsgBlock ssc) where
    arbitrary = genericArbitrary
    shrink = genericShrink

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
    :: (Arbitrary (SscPayload ssc), SscHelpersClass ssc)
    => Bool -- ^ Whether to create genesis block before creating main block for 0th slot
    -> [Either SecretKey (SecretKey, SecretKey, Bool)]
    -> [T.SlotId]
    -> [T.BlockHeader ssc]
    -> Gen [T.BlockHeader ssc]
recursiveHeaderGen genesis
                   (eitherOfLeader : leaders)
                   (T.SlotId{..} : rest)
                   blockchain
    | genesis && T.getSlotIndex siSlot == 0 = do
          gBody <- arbitrary
          let gHeader = Left $ T.mkGenesisHeader (head blockchain) siEpoch gBody
          mHeader <- genMainHeader (Just gHeader)
          recursiveHeaderGen True leaders rest (mHeader : gHeader : blockchain)
    | otherwise = do
          curHeader <- genMainHeader (head blockchain)
          recursiveHeaderGen True leaders rest (curHeader : blockchain)
  where
    genMainHeader prevHeader = do
        body <- arbitrary
        extraHData <- arbitrary
        lowEpoch <- choose (0, siEpoch)
        highEpoch <- choose (siEpoch, bhlMaxStartingEpoch + 5)
        -- These two values may not be used at all. If the slot in question
        -- will have a simple signature, laziness will prevent them from
        -- being calculated. Otherwise, they'll be the proxy secret key's ω.
        let slotId = T.SlotId siEpoch siSlot
            (leader, proxySK) = case eitherOfLeader of
                Left sk -> (sk, Nothing)
                Right (issuerSK, delegateSK, isSigEpoch) ->
                    let w = (lowEpoch, highEpoch)
                        delegatePK = toPublic delegateSK
                        toPsk :: Bi w => w -> ProxySecretKey w
                        toPsk = createPsk issuerSK delegatePK
                        proxy = if isSigEpoch
                                then Right (toPsk siEpoch, toPublic issuerSK)
                                else Left $ toPsk w
                    in (delegateSK, Just proxy)
        pure $ Right $
            T.mkMainHeader prevHeader slotId leader proxySK body extraHData
recursiveHeaderGen _ [] _ b = return b
recursiveHeaderGen _ _ [] b = return b


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
instance (Arbitrary (SscPayload ssc), SscHelpersClass ssc) =>
         Arbitrary (BlockHeaderList ssc) where
    arbitrary = do
        incompleteEpochSize <- choose (1, epochSlots - 1)
        let slot = T.SlotId 0 minBound
        generateBHL True slot (epochSlots * bhlEpochs + incompleteEpochSize)

generateBHL
    :: (Arbitrary (SscPayload ssc), SscHelpersClass ssc)
    => Bool         -- ^ Whether to create genesis block before creating main
                    --    block for 0th slot
    -> T.SlotId     -- ^ Start slot
    -> T.SlotCount  -- ^ Slot count
    -> Gen (BlockHeaderList ssc)
generateBHL createInitGenesis startSlot slotCount = BHL <$> do
    let correctLeaderGen :: Gen (Either SecretKey (SecretKey, SecretKey, Bool))
        correctLeaderGen =
            -- We don't want to create blocks with self-signed psks
            let issDelDiff (Left _)        = True
                issDelDiff (Right (i,d,_)) = i /= d
            in arbitrary `suchThat` issDelDiff
    leadersList <- vectorOf (fromIntegral slotCount) correctLeaderGen
    let actualLeaders = map (toPublic . either identity (view _1)) leadersList
        slotIdsRange =
            take (fromIntegral slotCount) $
            map T.unflattenSlotId [T.flattenSlotId startSlot ..]
    (, actualLeaders) <$>
        recursiveHeaderGen
            createInitGenesis
            leadersList
            slotIdsRange
            []

-- | This type is used to generate a valid blockheader and associated header
-- verification params. With regards to the block header function
-- 'Pos.Types.Blocks.Functions.verifyHeader', the blockheaders that may be
-- part of the verification parameters are guaranteed to be valid, as are the
-- slot leaders and the current slot.
newtype HeaderAndParams ssc = HAndP
    { getHAndP :: (T.VerifyHeaderParams ssc, T.BlockHeader ssc)
    } deriving (Eq, Show)

-- | A lot of the work to generate a valid sequence of blockheaders has
-- already been done in the 'Arbitrary' instance of the 'BlockHeaderList'
-- type, so it is used here and at most 3 blocks are taken from the generated
-- list.
instance (Arbitrary (SscPayload ssc), SscHelpersClass ssc) =>
    Arbitrary (HeaderAndParams ssc) where
    arbitrary = do
        -- This integer is used as a seed to randomly choose a slot down below
        seed <- arbitrary :: Gen Int
        startSlot <- T.SlotId <$> choose (0, bhlMaxStartingEpoch) <*> arbitrary
        (headers, leaders) <- first reverse . getHeaderList <$> (generateBHL True startSlot =<< choose (1, 2))
        let num = length headers
        -- 'skip' is the random number of headers that should be skipped in
        -- the header chain. This ensures different parts of it are chosen
        -- each time.
        skip <- choose (0, num - 1)
        let atMost2HeadersAndLeaders = take 2 $ drop skip headers
            (prev, header) =
                case atMost2HeadersAndLeaders of
                    [h] -> (Nothing, h)
                    [h1, h2] -> (Just h1, h2)
                    _ -> error "[BlockSpec] the headerchain doesn't have enough headers"
            -- This binding captures the chosen header's epoch. It is used to
            -- drop all all leaders of headers from previous epochs.
            thisEpochStartIndex = fromIntegral epochSlots *
                                  fromIntegral (header ^. T.epochIndexL)
            thisHeadersEpoch = drop thisEpochStartIndex leaders
            -- A helper function. Given integers 'x' and 'y', it chooses a
            -- random integer in the interval [x, y]
            betweenXAndY x y = fst . randomR (x, y) . mkStdGen $ seed
            -- One of the fields in the 'VerifyHeaderParams' type is 'Just
            -- SlotId'. The following binding is where it is calculated.
            randomSlotBeforeThisHeader =
                case header of
                    -- If the header is of the genesis kind, this field is
                    -- not needed.
                    Left _  -> Nothing
                    -- If it's a main blockheader, then a valid "current"
                    -- SlotId for testing is any with an epoch greater than
                    -- the header's epoch and with any slot index, or any in
                    -- the same epoch but with a greater or equal slot index
                    -- than the header.
                    Right h -> -- Nothing {-
                        let (T.SlotId e s) = view Core.headerSlotL h
                            rndEpoch :: Core.EpochIndex
                            rndEpoch = betweenXAndY e maxBound
                            rndSlotIdx :: Core.LocalSlotIndex
                            rndSlotIdx = if rndEpoch > e
                                then betweenXAndY minBound maxBound
                                else betweenXAndY s maxBound
                            rndSlot = T.SlotId rndEpoch rndSlotIdx
                        in Just rndSlot
            hasUnknownAttributes =
                not . null $
                either
                    (view $ Core.gbhExtra . T.gehAttributes . to attrRemain)
                    (view $ Core.gbhExtra . T.mehAttributes . to attrRemain)
                    header
            params = T.VerifyHeaderParams
                { T.vhpPrevHeader = prev
                , T.vhpCurrentSlot = randomSlotBeforeThisHeader
                , T.vhpLeaders = nonEmpty $ map T.addressHash thisHeadersEpoch
                , T.vhpMaxSize = Just (biSize header)
                , T.vhpVerifyNoUnknown = not hasUnknownAttributes
                }
        return . HAndP $ (params, header)
