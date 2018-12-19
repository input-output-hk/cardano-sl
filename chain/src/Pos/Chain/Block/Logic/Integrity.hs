{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Verification of headers and blocks, also chain integrity
-- checks. Almost pure (requires leaders to be explicitly passed).

module Pos.Chain.Block.Logic.Integrity
       (
         -- * Header
         VerifyHeaderParams (..)
       , verifyHeader
       , verifyHeaders

         -- * Block
       , VerifyBlockParams (..)
       , verifyBlock
       , verifyBlocks
       ) where


import           Universum

import           Control.Lens (ix)
import           Formatting (build, int, sformat, shown, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util (VerificationRes (..), verifyGeneric)

import qualified Pos.Binary.Class as Bi
import           Pos.Chain.Block.Block (Block, gbExtra, genBlockLeaders,
                     getBlockHeader, verifyBlockInternal)
import           Pos.Chain.Block.Genesis (gebAttributes, gehAttributes)
import           Pos.Chain.Block.HasPrevBlock (prevBlockL)
import           Pos.Chain.Block.Header (BlockHeader (..), HasHeaderHash (..),
                     HeaderHash, blockHeaderProtocolMagicId, gbhExtra,
                     mainHeaderLeaderKey, verifyBlockHeader)
import           Pos.Chain.Block.IsHeader (headerSlotL)
import           Pos.Chain.Block.Main (mebAttributes, mehAttributes)
import           Pos.Chain.Block.Slog (LastBlkSlots, LastSlotInfo (..))
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (TxValidationRules)
import           Pos.Chain.Update (BlockVersionData (..), ConsensusEra (..),
                     ObftConsensusStrictness (..))
import           Pos.Core (BlockCount (..), ChainDifficulty, EpochOrSlot (..),
                     HasDifficulty (..), HasEpochIndex (..),
                     HasEpochOrSlot (..), LocalSlotIndex (..), SlotId (..),
                     SlotLeaders, addressHash, getSlotIndex)
import           Pos.Core.Attributes (areAttributesKnown)
import           Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Core.Slotting (EpochIndex (..))
import           Pos.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     getProtocolMagic)

----------------------------------------------------------------------------
-- Header
----------------------------------------------------------------------------

-- Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficultyIncrement :: BlockHeader -> ChainDifficulty
headerDifficultyIncrement (BlockHeaderGenesis _) = 0
headerDifficultyIncrement (BlockHeaderMain _)    = 1

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderParams = VerifyHeaderParams
    { vhpPrevHeader       :: !(Maybe BlockHeader)
      -- ^ Nothing means that block is unknown, not genesis.
    , vhpCurrentSlot      :: !(Maybe SlotId)
      -- ^ Current slot is used to check whether header is not from future.
    , vhpLeaders          :: !(Maybe SlotLeaders)
      -- ^ Set of leaders for the epoch related block is from.
    , vhpMaxSize          :: !(Maybe Byte)
      -- ^ Maximal allowed header size. It's applied to 'BlockHeader'.
    , vhpVerifyNoUnknown  :: !Bool
      -- ^ Check that header has no unknown attributes.
    , vhpConsensusEra     :: !ConsensusEra
      -- ^ Used to perform specific header verification logic depending on the consensus era
    , vhpLastBlkSlotsAndK :: !(Maybe (LastBlkSlots, BlockCount))
      -- ^ The security parameter, `k`, and the slot leaders that signed the
      -- last `k` blocks.
    } deriving (Eq, Show, Generic)

instance NFData VerifyHeaderParams

verifyFromEither :: Text -> Either Text b -> VerificationRes
verifyFromEither txt (Left reason) = verifyGeneric [(False, txt <> ": " <> reason)]
verifyFromEither txt (Right _)     = verifyGeneric [(True, txt)]

-- CHECK: @verifyHeader
-- | Check some predicates (determined by 'VerifyHeaderParams') about
-- 'BlockHeader'.
--
-- Supported checks:
-- 1.  Checks with respect to the preceding block:
--     1.  If the new block is a genesis one, difficulty does not increase.
--         Otherwise, it increases by one.
--     2.  Hashing the preceding block's header yields the same value as the one
--         stored in the new block's header.
--     3.  Corresponding `EpochOrSlot`s strictly increase.
--     4.  If the new block is a main one, its epoch is equal to the epoch of the
--         preceding block.
-- 2.  The block's slot does not exceed the current slot.
-- 3.  The block's leader is expected (matches either the corresponding leader from
--     the initial leaders or a leader from one of the preceding genesis blocks).
-- 4.  Header size does not exceed `bvdMaxHeaderSize`.
-- 5.  (Optional) Header has no unknown attributes.
verifyHeader
    :: ProtocolMagic -> VerifyHeaderParams -> BlockHeader -> VerificationRes
verifyHeader pm VerifyHeaderParams {..} h =
       verifyFromEither "internal header consistency" (verifyBlockHeader pm h)
    <> verifyGeneric checks
  where
    checks =
        mconcat
            [ checkProtocolMagicId
            , maybe mempty relatedToPrevHeader vhpPrevHeader
            , maybe mempty relatedToCurrentSlot vhpCurrentSlot
            , maybe2 mempty relatedToLeaders vhpLeaders vhpLastBlkSlotsAndK
            , checkSize
            , bool mempty (verifyNoUnknown h) vhpVerifyNoUnknown
            ]
    -- | Takes a default value, a function, and two Maybe values. If any of the
    -- Maybe values is Nothing, the function returns the default value.
    -- Otherwise, it applies the function to the values inside the two Justs
    -- and returns the result.
    maybe2 :: c -> (a -> b -> c) -> Maybe a -> Maybe b -> c
    maybe2 c _ Nothing _          = c
    maybe2 c _ _ Nothing          = c
    maybe2 _ fn (Just a) (Just b) = fn a b
    --
    checkHash :: HeaderHash -> HeaderHash -> (Bool, Text)
    checkHash expectedHash actualHash =
        ( expectedHash == actualHash
        , sformat
              ("inconsistent hash (expected "%build%", found "%build%")")
              expectedHash
              actualHash)
    checkDifficulty :: ChainDifficulty -> ChainDifficulty -> (Bool, Text)
    checkDifficulty expectedDifficulty actualDifficulty =
        ( expectedDifficulty == actualDifficulty
        , sformat
              ("incorrect difficulty (expected "%int%", found "%int%")")
              expectedDifficulty
              actualDifficulty)
    checkEpochOrSlot :: EpochOrSlot -> EpochOrSlot -> (Bool, Text)
    checkEpochOrSlot oldEOS newEOS =
        ( oldEOS < newEOS
        , sformat
              ("slots are not monotonic ("%build%" >= "%build%")")
              oldEOS newEOS
        )
    sameEpoch :: EpochIndex -> EpochIndex -> (Bool, Text)
    sameEpoch oldEpoch newEpoch =
        ( oldEpoch == newEpoch
        , sformat
              ("two adjacent blocks are from different epochs ("%build%" != "%build%")")
              oldEpoch newEpoch
        )
    checkProtocolMagicId =
        [ ( getProtocolMagicId pm == blockHeaderProtocolMagicId h
          , sformat
                ("protocol magic number mismatch: got "%int%" but expected "%int)
                (unProtocolMagicId (blockHeaderProtocolMagicId h))
                (getProtocolMagic pm)
          )
        ]
    checkSize =
        case vhpMaxSize of
            Nothing -> mempty
            -- FIXME do not use 'biSize'! It's expensive.
            Just maxSize ->
                [ ( Bi.biSize h <= maxSize
                  , sformat
                        ("header's size exceeds limit ("%memory%" > "%memory%")")
                        (Bi.biSize h)
                        maxSize)
                ]

    -- CHECK: Performs checks related to the previous header:
    --
    --   * Difficulty is correct.
    --   * Hash is correct.
    --   * Epoch/slot are consistent.
    relatedToPrevHeader :: BlockHeader -> [(Bool, Text)]
    relatedToPrevHeader prevHeader =
        [ checkDifficulty
              (prevHeader ^. difficultyL + headerDifficultyIncrement h)
              (h ^. difficultyL)
        -- This \/ checks that the previous block's header matches what our current block
        -- says the previous header hash was. This verifies integrity of the chain.
        , checkHash
              (headerHash prevHeader)
              (h ^. prevBlockL)
        , checkEpochOrSlot (getEpochOrSlot prevHeader) (getEpochOrSlot h)
        , case h of
              BlockHeaderGenesis _ -> (True, "") -- check that epochId prevHeader < epochId h performed above
              BlockHeaderMain _    -> case vhpConsensusEra of
                Original -> sameEpoch (prevHeader ^. epochIndexL) (h ^. epochIndexL)
                OBFT _   -> case unEpochOrSlot (getEpochOrSlot h) of
                    Left _ -> (True, "we received an Epoch Boundary Block in OBFT era, which shouldn't \
                                     \happen, but we are passing")
                    Right sid -> case (getEpochIndex (siEpoch sid), getSlotIndex (siSlot sid)) of
                        (0, 0) -> (True, "first MainBlock of first epoch: no previous block")
                        -- First block of the epoch: assert its epoch is 1 greater than
                        -- the epoch of the prior block.
                        (_, 0) -> sameEpoch (1 + prevHeader ^. epochIndexL) (h ^. epochIndexL)
                        -- A block somewhere else in the epoch: assert it has the same
                        -- epoch as prior block.
                        _      -> sameEpoch (prevHeader ^. epochIndexL)     (h ^. epochIndexL)
        ]

    -- CHECK: Verifies that the slot does not lie in the future.
    relatedToCurrentSlot :: SlotId -> [(Bool, Text)]
    relatedToCurrentSlot curSlotId =
        case h of
            BlockHeaderGenesis _ -> [(True, "block is from slot which hasn't happened yet")]
            BlockHeaderMain bh   ->
                [
                    ( (bh ^. headerSlotL) <= curSlotId
                    , sformat ("block is from slot "%build%" which hasn't happened yet (current slot "%build%")") (bh ^. headerSlotL) curSlotId
                    )
                ]

    -- CHECK: Checks that the block leader is the expected one.
    relatedToLeaders leaders (OldestFirst lastBlkSlots, blkSecurityParam) =
        case h of
            BlockHeaderGenesis _ -> []
            BlockHeaderMain mainHeader -> case vhpConsensusEra of
                -- For the `OBFT ObftLenient` era, we only check whether the
                -- block's creator is an "acceptable" slot leader (one of the
                -- genesis stakeholders). So, in this case, `leaders`
                -- represents a collection of acceptable slot leaders and not
                -- a slot leader schedule as it would for the `OBFT ObftStrict`
                -- and `Original` cases.
                OBFT ObftLenient ->
                    let slotLeader = addressHash $ mainHeader ^. mainHeaderLeaderKey
                    in [  ( (slotLeader `elem` leaders)
                          , sformat ("slot's leader, "%build%", is not an acceptable leader. acceptableLeaders: "%shown)
                                slotLeader
                                leaders)
                        , ( (obftLeaderCanMint slotLeader)
                          , sformat ("slot's leader, "%build%", has minted too many blocks in the past "%build%" slots.")
                                slotLeader
                                k)
                        ]

                OBFT ObftStrict ->
                    let slotIndex = getSlotIndex $ siSlot $ mainHeader ^. headerSlotL
                        slotLeader = leaders ^? ix (fromIntegral slotIndex)
                        expectedSlotLeader = addressHash $ mainHeader ^. mainHeaderLeaderKey
                    in [  ( (Just expectedSlotLeader == slotLeader)
                          , sformat ("slot's leader, "%build%", is different from expected one, "%build%". slotIndex: "%build%", leaders: "%shown)
                                slotLeader
                                expectedSlotLeader
                                slotIndex
                                leaders)
                        , ( (obftLeaderCanMint expectedSlotLeader)
                          , sformat ("slot's leader, "%build%", has minted too many blocks in the past "%build%" slots.")
                                slotLeader
                                k)
                        ]

                Original ->
                    let slotIndex = getSlotIndex $ siSlot $ mainHeader ^. headerSlotL
                        slotLeader = leaders ^? ix (fromIntegral slotIndex)
                        expectedSlotLeader = addressHash $ mainHeader ^. mainHeaderLeaderKey
                    in [ ( (Just expectedSlotLeader == slotLeader)
                        , sformat ("slot's leader, "%build%", is different from expected one, "%build%". slotIndex: "%build%", leaders: "%shown)
                                slotLeader
                                expectedSlotLeader
                                slotIndex
                                leaders)
                        ]
      where
        -- Determine whether the leader is allowed to mint a block based on
        -- whether blocksMintedByLeaderInLastKSlots <= floor (k * t)
        obftLeaderCanMint leaderAddrHash =
            (blocksMintedByLeaderInLastKSlots leaderAddrHash)
                <= leaderMintThreshold
        --
        blocksMintedByLeaderInLastKSlots leaderAddrHash =
            length $
                filter (\lsi -> leaderAddrHash == (addressHash $ lsiLeaderPubkeyHash lsi))
                       lastBlkSlots
        --
        leaderMintThreshold :: Int
        leaderMintThreshold = floor $ (fromIntegral k :: Double) * t
        --
        t :: Double
        t = 0.22
        --
        k :: Word64
        k = getBlockCount blkSecurityParam

    verifyNoUnknown (BlockHeaderGenesis genH) =
        let attrs = genH ^. gbhExtra . gehAttributes
        in  [ ( areAttributesKnown attrs
              , sformat ("genesis header has unknown attributes: "%build) attrs)
            ]
    verifyNoUnknown (BlockHeaderMain mainH) =
        let attrs = mainH ^. gbhExtra . mehAttributes
        in [ ( areAttributesKnown attrs
             , sformat ("main header has unknown attributes: "%build) attrs)
           ]

-- | Verifies a set of block headers. Only basic consensus check and
-- linking checks are performed!
verifyHeaders ::
       ProtocolMagic
    -> ConsensusEra
    -> Maybe (LastBlkSlots, BlockCount)
    -> Maybe SlotLeaders
    -> NewestFirst [] BlockHeader
    -> VerificationRes
verifyHeaders _ _ _ _ (NewestFirst []) = mempty
verifyHeaders pm era lastBlkSlotsAndK leaders (NewestFirst (headers@(_:xh))) =
    snd $
    foldr foldFoo (leaders,mempty) $ headers `zip` (map Just xh ++ [Nothing])
  where
    foldFoo (cur,prev) (prevLeaders,res) =
        let curLeaders = case cur of
                             -- we don't know leaders for the next epoch
                             BlockHeaderGenesis _ -> Nothing
                             _                    -> prevLeaders

        in (curLeaders, verifyHeader pm (toVHP curLeaders prev) cur <> res)
    toVHP l p =
        VerifyHeaderParams
        { vhpPrevHeader = p
        , vhpCurrentSlot = Nothing
        , vhpLeaders = l
        , vhpMaxSize = Nothing
        , vhpVerifyNoUnknown = False
        , vhpConsensusEra = era
        , vhpLastBlkSlotsAndK = lastBlkSlotsAndK
        }

----------------------------------------------------------------------------
-- Block
----------------------------------------------------------------------------

-- | Parameters of Block static verification. This type contains all data
-- necessary for verification of a single block.
-- Note: to check that block references previous block and/or is referenced
-- by next block, use header verification (via vbpVerifyHeader).
data VerifyBlockParams = VerifyBlockParams
    { vbpVerifyHeader    :: !VerifyHeaderParams
      -- ^ Verifies header accordingly to params ('verifyHeader')
    , vbpMaxSize         :: !Byte
    -- ^ Maximal block size. This value limit size of 'Block' (which
    -- is either main or genesis block).
    , vbpVerifyNoUnknown :: !Bool
    -- ^ Check that block has no unknown attributes.
    } deriving (Generic)

instance NFData VerifyBlockParams

-- CHECK: @verifyBlock
-- | Check predicates defined by VerifyBlockParams.
-- #verifyHeader
--
-- Supported checks:
--
-- 1.  All checks related to the header.
-- 2.  The size of each block does not exceed `bvdMaxBlockSize`.
-- 3.  (Optional) No block has any unknown attributes.
verifyBlock
    :: Genesis.Config
    -> ConsensusEra
    -> TxValidationRules
    -> VerifyBlockParams
    -> Block
    -> VerificationRes
verifyBlock genesisConfig era txValRules VerifyBlockParams {..} blk = mconcat
    [ verifyFromEither "internal block consistency"
                       (verifyBlockInternal genesisConfig era txValRules blk)
    , verifyHeader (configProtocolMagic genesisConfig)
                   vbpVerifyHeader
                   (getBlockHeader blk)
    , checkSize vbpMaxSize
    , bool mempty (verifyNoUnknown blk) vbpVerifyNoUnknown
    ]
  where
    -- Oh no! Verification involves re-searilizing the thing!
    -- What a tragic waste.
    -- What shall we do about this?
    blkSize = Bi.biSize blk
    checkSize maxSize = verifyGeneric [
      (blkSize <= maxSize,
       sformat ("block's size exceeds limit ("%memory%" > "%memory%")")
       blkSize maxSize)
      ]
    verifyNoUnknown (Left genBlk) =
        let attrs = genBlk ^. gbExtra . gebAttributes
        in verifyGeneric
               [ ( areAttributesKnown attrs
                 , sformat ("genesis block has unknown attributes: "%build) attrs)
               ]
    verifyNoUnknown (Right mainBlk) =
        let attrs = mainBlk ^. gbExtra . mebAttributes
        in verifyGeneric
               [ ( areAttributesKnown attrs
                 , sformat ("main block has unknown attributes: "%build) attrs)
               ]

-- Type alias for the fold accumulator used inside 'verifyBlocks'
type VerifyBlocksIter = (SlotLeaders, Maybe BlockHeader, VerificationRes)

-- CHECK: @verifyBlocks
-- Verifies a sequence of blocks.
-- #verifyBlock
-- | Verify a sequence of blocks.
--
-- Block verification consists of header verification and body verification.
-- See 'verifyHeader' and 'verifyBlock' for more information.
--
-- foldl' is used here which eliminates laziness of triple. It doesn't affect
-- laziness of 'VerificationRes' which is good because laziness for this data
-- type is crucial.
verifyBlocks
    :: Genesis.Config
    -> ConsensusEra
    -> TxValidationRules
    -> Maybe (LastBlkSlots, BlockCount)
    -> Maybe SlotId
    -> Bool
    -> BlockVersionData
    -> SlotLeaders
    -> OldestFirst [] Block
    -> VerificationRes
verifyBlocks genesisConfig
             era
             txValRules
             lastBlkSlotsAndK
             curSlotId
             verifyNoUnknown
             bvd
             initLeaders = view _3 . foldl' step start
  where
    start :: VerifyBlocksIter
    -- Note that here we never know previous header before this
    -- function is launched.  Which means that we will not do any
    -- checks related to previous header. And it is fine, because we
    -- must do these checks in advance, when we are processing
    -- headers. However, it's a little obscure invariant, so keep it
    -- in mind.
    start = (initLeaders, Nothing, mempty)
    step :: VerifyBlocksIter -> Block -> VerifyBlocksIter
    step (leaders, prevHeader, res) blk =
        let newLeaders = case blk of
                Left genesisBlock -> genesisBlock ^. genBlockLeaders
                Right _           -> leaders
            blockMaxSize = case blk of
                Left _  -> 2000000
                Right _ -> bvdMaxBlockSize bvd
            vhp =
                VerifyHeaderParams
                { vhpPrevHeader = prevHeader
                , vhpLeaders = Just newLeaders
                , vhpCurrentSlot = curSlotId
                , vhpMaxSize = Just (bvdMaxHeaderSize bvd)
                , vhpVerifyNoUnknown = verifyNoUnknown
                , vhpConsensusEra = era
                , vhpLastBlkSlotsAndK = lastBlkSlotsAndK
                }
            vbp =
                VerifyBlockParams
                { vbpVerifyHeader = vhp
                , vbpMaxSize = blockMaxSize
                , vbpVerifyNoUnknown = verifyNoUnknown
                }
        in (newLeaders, Just $ getBlockHeader blk, res <> verifyBlock genesisConfig era txValRules vbp blk)
