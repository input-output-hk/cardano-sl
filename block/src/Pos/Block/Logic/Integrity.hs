{-# LANGUAGE TypeFamilies #-}

-- | Verification of headers and blocks, also chain integrity
-- checks. Almost pure (requires leaders to be explicitly passed).

module Pos.Block.Logic.Integrity
       (
         -- * Header
         VerifyHeaderParams (..)
       , verifyHeader
       , verifyHeaders

         -- * Block
       , VerifyBlockParams (..)
       , verifyBlocks
       ) where


import           Universum

import           Control.Lens (ix)
import           Formatting (build, int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util (VerificationRes (..), verifyGeneric)

import qualified Pos.Binary.Class as Bi
import           Pos.Binary.Core ()
import           Pos.Binary.Update ()
import qualified Pos.Block.BHelpers as BHelpers
import           Pos.Core (BlockVersionData (..), ChainDifficulty, EpochOrSlot,
                           HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                           HasHeaderHash (..), HeaderHash, SlotId (..), SlotLeaders,
                           protocolMagic, addressHash, gbExtra, gbhExtra, getSlotIndex,
                           headerSlotL, prevBlockL, HasProtocolConstants, HasProtocolMagic)
import           Pos.Core.Block (Block, BlockHeader (..), blockHeaderProtocolMagic,
                                 gebAttributes, gehAttributes, genBlockLeaders,
                                 getBlockHeader, mainHeaderLeaderKey,
                                 mebAttributes, mehAttributes)
import           Pos.Crypto (ProtocolMagic (getProtocolMagic))
import           Pos.Data.Attributes (areAttributesKnown)
import           Pos.Util.Chrono (NewestFirst (..), OldestFirst)

----------------------------------------------------------------------------
-- Header
----------------------------------------------------------------------------

-- Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficultyIncrement :: BlockHeader -> ChainDifficulty
headerDifficultyIncrement (BlockHeaderGenesis _) = 0
headerDifficultyIncrement (BlockHeaderMain _)    = 1

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderParams = VerifyHeaderParams
    { vhpPrevHeader      :: !(Maybe BlockHeader)
      -- ^ Nothing means that block is unknown, not genesis.
    , vhpCurrentSlot     :: !(Maybe SlotId)
      -- ^ Current slot is used to check whether header is not from future.
    , vhpLeaders         :: !(Maybe SlotLeaders)
      -- ^ Set of leaders for the epoch related block is from.
    , vhpMaxSize         :: !(Maybe Byte)
      -- ^ Maximal allowed header size. It's applied to 'BlockHeader'.
    , vhpVerifyNoUnknown :: !Bool
      -- ^ Check that header has no unknown attributes.
    } deriving (Eq, Show)

verifyFromEither :: Text -> Either Text b -> VerificationRes
verifyFromEither txt (Left reason)  = verifyGeneric [(False, txt <> ": " <> reason)]
verifyFromEither txt (Right _) = verifyGeneric [(True, txt)]

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
    :: HasProtocolMagic
    => VerifyHeaderParams -> BlockHeader -> VerificationRes
verifyHeader VerifyHeaderParams {..} h =
       verifyFromEither "internal header consistency" (BHelpers.verifyBlockHeader h)
    <> verifyGeneric checks
  where
    checks =
        mconcat
            [ checkProtocolMagic
            , maybe mempty relatedToPrevHeader vhpPrevHeader
            , maybe mempty relatedToCurrentSlot vhpCurrentSlot
            , maybe mempty relatedToLeaders vhpLeaders
            , checkSize
            , bool mempty (verifyNoUnknown h) vhpVerifyNoUnknown
            ]
    checkHash :: HeaderHash -> HeaderHash -> (Bool, Text)
    checkHash expectedHash actualHash =
        ( expectedHash == actualHash
        , sformat
              ("inconsistent hash (expected "%build%", found "%build%")")
              expectedHash
              actualHash)
    checkDifficulty expectedDifficulty actualDifficulty =
        ( expectedDifficulty == actualDifficulty
        , sformat
              ("incorrect difficulty (expected "%int%", found "%int%")")
              expectedDifficulty
              actualDifficulty)
    checkSlot :: EpochOrSlot -> EpochOrSlot -> (Bool, Text)
    checkSlot oldSlot newSlot =
        ( oldSlot < newSlot
        , sformat
              ("slots are not monotonic ("%build%" >= "%build%")")
              oldSlot newSlot
        )
    sameEpoch oldEpoch newEpoch =
        ( oldEpoch == newEpoch
        , sformat
              ("two adjacent blocks are from different epochs ("%build%" != "%build%")")
              oldEpoch newEpoch
        )
    -- FIXME do not use 'HasConfigurations' 'protocolMagic'. Take it as a
    -- parameter instead.
    checkProtocolMagic =
        [ ( protocolMagic == blockHeaderProtocolMagic h
          , sformat
                ("protocol magic number mismatch: got "%int%" but expected "%int)
                (getProtocolMagic (blockHeaderProtocolMagic h))
                (getProtocolMagic protocolMagic)
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
    relatedToPrevHeader prevHeader =
        [ checkDifficulty
              (prevHeader ^. difficultyL + headerDifficultyIncrement h)
              (h ^. difficultyL)
        , checkHash
              (headerHash prevHeader)
              (h ^. prevBlockL)
        , checkSlot (getEpochOrSlot prevHeader) (getEpochOrSlot h)
        , case h of
              BlockHeaderGenesis _ -> (True, "") -- check that epochId prevHeader < epochId h performed above
              BlockHeaderMain _    -> sameEpoch (prevHeader ^. epochIndexL) (h ^. epochIndexL)
        ]

    -- CHECK: Verifies that the slot does not lie in the future.
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
    relatedToLeaders leaders =
        case h of
            BlockHeaderGenesis _ -> []
            BlockHeaderMain mainHeader ->
                [ ( (Just (addressHash $ mainHeader ^. mainHeaderLeaderKey) ==
                     leaders ^?
                     ix (fromIntegral $ getSlotIndex $
                         siSlot $ mainHeader ^. headerSlotL))
                  , "block's leader is different from expected one")
                ]

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
       HasProtocolMagic
    => Maybe SlotLeaders
    -> NewestFirst [] BlockHeader
    -> VerificationRes
verifyHeaders _ (NewestFirst []) = mempty
verifyHeaders leaders (NewestFirst (headers@(_:xh))) =
    snd $
    foldr foldFoo (leaders,mempty) $ headers `zip` (map Just xh ++ [Nothing])
  where
    foldFoo (cur,prev) (prevLeaders,res) =
        let curLeaders = case cur of
                             -- we don't know leaders for the next epoch
                             BlockHeaderGenesis _ -> Nothing
                             _                    -> prevLeaders

        in (curLeaders, verifyHeader (toVHP curLeaders prev) cur <> res)
    toVHP l p =
        VerifyHeaderParams
        { vhpPrevHeader = p
        , vhpCurrentSlot = Nothing
        , vhpLeaders = l
        , vhpMaxSize = Nothing
        , vhpVerifyNoUnknown = False
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
    }

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
    :: (HasProtocolConstants, HasProtocolMagic)
    => VerifyBlockParams -> Block -> VerificationRes
verifyBlock VerifyBlockParams {..} blk = mconcat
    [ verifyFromEither "internal block consistency" (BHelpers.verifyBlock blk)
    , verifyHeader vbpVerifyHeader (getBlockHeader blk)
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
    :: ( HasProtocolConstants
       , HasProtocolMagic
       )
    => Maybe SlotId
    -> Bool
    -> BlockVersionData
    -> SlotLeaders
    -> OldestFirst [] Block
    -> VerificationRes
verifyBlocks curSlotId verifyNoUnknown bvd initLeaders = view _3 . foldl' step start
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
            vhp =
                VerifyHeaderParams
                { vhpPrevHeader = prevHeader
                , vhpLeaders = Just newLeaders
                , vhpCurrentSlot = curSlotId
                , vhpMaxSize = Just (bvdMaxHeaderSize bvd)
                , vhpVerifyNoUnknown = verifyNoUnknown
                }
            vbp =
                VerifyBlockParams
                { vbpVerifyHeader = vhp
                , vbpMaxSize = bvdMaxBlockSize bvd
                , vbpVerifyNoUnknown = verifyNoUnknown
                }
        in (newLeaders, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)
