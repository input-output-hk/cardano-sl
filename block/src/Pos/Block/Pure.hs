{-# LANGUAGE TypeFamilies #-}

-- | Pure functions related to blocks and headers.

module Pos.Block.Pure
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
import           Pos.Block.BHelpers ()
import           Pos.Core (BlockVersionData (..), ChainDifficulty, EpochOrSlot, HasConfiguration,
                           HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                           HasHeaderHash (..), HeaderHash, SlotId (..), SlotLeaders, addressHash,
                           gbExtra, gbhExtra, getSlotIndex, headerSlotL, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader, gebAttributes, gehAttributes, genBlockLeaders,
                                 getBlockHeader, mainHeaderLeaderKey, mebAttributes, mehAttributes)
import           Pos.Data.Attributes (areAttributesKnown)
import           Pos.Util.Chrono (NewestFirst (..), OldestFirst)

----------------------------------------------------------------------------
-- Header
----------------------------------------------------------------------------

-- Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficultyIncrement :: BlockHeader -> ChainDifficulty
headerDifficultyIncrement (Left _)  = 0
headerDifficultyIncrement (Right _) = 1

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderParams = VerifyHeaderParams
    { vhpPrevHeader      :: !(Maybe BlockHeader)
      -- ^ Nothing means that block is unknown, not genesis.
    , vhpCurrentSlot     :: !(Maybe SlotId)
      -- ^ Current slot is used to check whether header is not from future.
    , vhpLeaders         :: !(Maybe SlotLeaders)
      -- ^ Set of leaders for the epoch related block is from
    , vhpMaxSize         :: !(Maybe Byte)
      -- ^ Maximal allowed header size. It's applied to 'BlockHeader'.
    , vhpVerifyNoUnknown :: !Bool
      -- ^ Check that header has no unknown attributes.
    } deriving (Eq, Show)

maybeMempty :: Monoid m => (a -> m) -> Maybe a -> m
maybeMempty = maybe mempty

-- CHECK: @verifyHeader
-- | Check some predicates (determined by 'VerifyHeaderParams') about
-- 'BlockHeader'.
verifyHeader
    :: HasConfiguration
    => VerifyHeaderParams -> BlockHeader -> VerificationRes
verifyHeader VerifyHeaderParams {..} h =
    verifyGeneric checks
  where
    checks =
        mconcat
            [ maybeMempty relatedToPrevHeader vhpPrevHeader
            , maybeMempty relatedToCurrentSlot vhpCurrentSlot
            , maybeMempty relatedToLeaders vhpLeaders
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
    checkSize =
        case vhpMaxSize of
            Nothing -> mempty
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
              Left  _ -> (True, "") -- check that epochId prevHeader < epochId h performed above
              Right _ -> sameEpoch (prevHeader ^. epochIndexL) (h ^. epochIndexL)
        ]

    -- CHECK: Verifies that the slot does not lie in the future.
    relatedToCurrentSlot curSlotId =
        [ ( either (const True) ((<= curSlotId) . view headerSlotL) h
          , "block is from slot which hasn't happened yet")
        ]

    -- CHECK: Checks that the block leader is the expected one.
    relatedToLeaders leaders =
        case h of
            Left _ -> []
            Right mainHeader ->
                [ ( (Just (addressHash $ mainHeader ^. mainHeaderLeaderKey) ==
                     leaders ^?
                     ix (fromIntegral $ getSlotIndex $
                         siSlot $ mainHeader ^. headerSlotL))
                  , "block's leader is different from expected one")
                ]

    verifyNoUnknown (Left genH) =
        let attrs = genH ^. gbhExtra . gehAttributes
        in  [ ( areAttributesKnown attrs
              , sformat ("genesis header has unknown attributes: "%build) attrs)
            ]
    verifyNoUnknown (Right mainH) =
        let attrs = mainH ^. gbhExtra . mehAttributes
        in [ ( areAttributesKnown attrs
             , sformat ("main header has unknown attributes: "%build) attrs)
           ]

-- | Verifies a set of block headers. Only basic consensus check and
-- linking checks are performed!
verifyHeaders ::
       HasConfiguration
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
                             (Left _) -> Nothing
                             _        -> prevLeaders

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
verifyBlock
    :: HasConfiguration
    => VerifyBlockParams -> Block -> VerificationRes
verifyBlock VerifyBlockParams {..} blk =
    mconcat
        [ verifyHeader vbpVerifyHeader (getBlockHeader blk)
        , checkSize vbpMaxSize
        , bool mempty (verifyNoUnknown blk) vbpVerifyNoUnknown
        ]
  where
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
-- foldl' is used here which eliminates laziness of triple. It doesn't affect
-- laziness of 'VerificationRes' which is good because laziness for this data
-- type is crucial.
verifyBlocks
    :: ( t ~ OldestFirst f Block
       , NontrivialContainer t
       , HasConfiguration
       )
    => Maybe SlotId
    -> Bool
    -> BlockVersionData
    -> SlotLeaders
    -> OldestFirst f Block
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
