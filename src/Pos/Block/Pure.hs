{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Pure functions related to blocks and headers.

module Pos.Block.Pure
       ( headerDifficultyIncrement

       -- * Header
       , VerifyHeaderParams (..)
       , verifyHeader
       , verifyHeaders

       -- * Block
       , VerifyBlockParams (..)
       , verifyBlocks
       ) where

import           Universum

import           Control.Lens               (ix, (%=))
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (partition)
import           Formatting                 (build, int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util.Verify       (VerificationRes (..), verifyGeneric)

import           Pos.Binary.Block.Core      ()
import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Core            ()
import           Pos.Binary.Update          ()
import           Pos.Block.Core             (BiSsc, Block, BlockHeader,
                                             BlockSignature (..), MainBlock, gbhConsensus,
                                             gebAttributes, gehAttributes,
                                             genBlockLeaders, getBlockHeader,
                                             mainBlockDlgPayload, mainHeaderLeaderKey,
                                             mcdSignature, mebAttributes, mehAttributes)
import           Pos.Core                   (BlockVersionData (..), ChainDifficulty,
                                             EpochOrSlot, HasDifficulty (..),
                                             HasEpochIndex (..), HasEpochOrSlot (..),
                                             HasHeaderHash (..), HeaderHash,
                                             ProxySKHeavyMap, SlotId (..), SlotLeaders,
                                             addressHash, gbExtra, gbhExtra, getSlotIndex,
                                             headerSlotL, prevBlockL)
import           Pos.Crypto                 (ProxySecretKey (..), pdCert)
import           Pos.Data.Attributes        (Attributes (attrRemain))
import           Pos.Delegation.Types       (getDlgPayload)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass)
import           Pos.Util.Chrono            (NewestFirst (..), OldestFirst)

----------------------------------------------------------------------------
-- Header
----------------------------------------------------------------------------

-- | Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficultyIncrement :: BlockHeader ssc -> ChainDifficulty
headerDifficultyIncrement (Left _)  = 0
headerDifficultyIncrement (Right _) = 1

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderParams ssc = VerifyHeaderParams
    { vhpPrevHeader      :: !(Maybe (BlockHeader ssc))
      -- ^ Nothing means that block is unknown, not genesis.
    , vhpCurrentSlot     :: !(Maybe SlotId)
      -- ^ Current slot is used to check whether header is not from future.
    , vhpLeaders         :: !(Maybe SlotLeaders)
      -- ^ Set of leaders for the epoch related block is from
    , vhpHeavyCerts      :: !(Maybe ProxySKHeavyMap)
      -- ^ Subset of heavy certs delegation map, where keys only
      -- contain public keys of block issuers passed to checking
      -- function.
    , vhpMaxSize         :: !(Maybe Byte)
      -- ^ Maximal allowed header size. It's applied to 'BlockHeader'.
    , vhpVerifyNoUnknown :: !Bool
      -- ^ Check that header has no unknown attributes.
    } deriving (Show, Eq)

maybeMempty :: Monoid m => (a -> m) -> Maybe a -> m
maybeMempty = maybe mempty

-- CHECK: @verifyHeader
-- | Check some predicates (determined by 'VerifyHeaderParams') about
-- 'BlockHeader'.
verifyHeader
    :: forall ssc . BiSsc ssc
    => VerifyHeaderParams ssc -> BlockHeader ssc -> VerificationRes
verifyHeader VerifyHeaderParams {..} h =
    verifyGeneric checks
  where
    checks =
        mconcat
            [ maybeMempty relatedToPrevHeader vhpPrevHeader
            , maybeMempty relatedToCurrentSlot vhpCurrentSlot
            , maybeMempty relatedToLeaders vhpLeaders
            , maybeMempty heavyCertValid vhpHeavyCerts
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
        in  [ ( null (attrRemain attrs)
              , sformat ("genesis header has unknown attributes: "%build) attrs)
            ]
    verifyNoUnknown (Right mainH) =
        let attrs = mainH ^. gbhExtra . mehAttributes
        in [ ( null (attrRemain attrs)
             , sformat ("main header has unknown attributes: "%build) attrs)
           ]

    heavyCertValid validCerts = flip (either mempty) h $ \h' ->
        case h' ^. gbhConsensus ^. mcdSignature of
            (BlockPSignatureHeavy proxySig) ->
                -- Block consensus public key is issuer's one, so we can
                -- use it to index heavy psks map.
                [ ( maybe False (\psk -> pskCert psk == pdCert proxySig) $
                    HM.lookup (h' ^. mainHeaderLeaderKey) validCerts
                  , sformat ("proxy signature's "%build%" related proxy cert "%
                             "can't be found/doesn't match the one in current "%
                             "allowed heavy psks set")
                            proxySig
                  ) ]
            _                               -> mempty

-- | Verifies a set of block headers. Only basic consensus check and
-- linking checks are performed!
verifyHeaders
    :: BiSsc ssc
    => NewestFirst [] (BlockHeader ssc) -> VerificationRes
verifyHeaders (NewestFirst []) = mempty
verifyHeaders (NewestFirst (headers@(_:xh))) = mconcat verified
  where
    verified =
        zipWith
            (\cur prev -> verifyHeader (toVHP prev) cur)
            headers
            (map Just xh ++ [Nothing])
    -- [CSL-1052] Consider doing more checks here.
    toVHP p =
        VerifyHeaderParams
        { vhpPrevHeader = p
        , vhpCurrentSlot = Nothing
        , vhpLeaders = Nothing
        , vhpHeavyCerts = Nothing
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
data VerifyBlockParams ssc = VerifyBlockParams
    { vbpVerifyHeader    :: !(VerifyHeaderParams ssc)
      -- ^ Verifies header accordingly to params ('verifyHeader')
    , vbpMaxSize         :: !Byte
    -- ^ Maximal block size. This value limit size of `Block` (which
    -- is either main or genesis block).
    , vbpVerifyNoUnknown :: !Bool
    -- ^ Check that block has no unknown attributes.
    }

-- CHECK: @verifyBlock
-- | Check predicates defined by VerifyBlockParams.
-- #verifyHeader
verifyBlock
    :: forall ssc. (SscHelpersClass ssc, BiSsc ssc)
    => VerifyBlockParams ssc -> Block ssc -> VerificationRes
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
               [ ( null (attrRemain attrs)
                 , sformat ("genesis block has unknown attributes: "%build) attrs)
               ]
    verifyNoUnknown (Right mainBlk) =
        let attrs = mainBlk ^. gbExtra . mebAttributes
        in verifyGeneric
               [ ( null (attrRemain attrs)
                 , sformat ("main block has unknown attributes: "%build) attrs)
               ]

-- Type alias for common type used inside 'verifyBlocks'
type VerifyBlocksIter ssc =
    (SlotLeaders, ProxySKHeavyMap, Maybe (BlockHeader ssc), VerificationRes)

-- Applies block certificates to 'ProxySKHeavyMap'. Used in
-- 'verifyBlocks'. It's not the best place for it here, but i can't
-- put it to "Delegation.Logic" because it leads to dependency cycle.
pskHeavyMapApplyBlock :: MainBlock ssc -> ProxySKHeavyMap -> ProxySKHeavyMap
pskHeavyMapApplyBlock block m = flip execState m $ do
    let (toDelete,toReplace) =
            partition (\ProxySecretKey{..} -> pskIssuerPk == pskDelegatePk)
                      (getDlgPayload $ view mainBlockDlgPayload block)
    for_ toDelete $ \psk -> identity %= HM.delete (pskIssuerPk psk)
    for_ toReplace $ \psk -> identity %= HM.insert (pskIssuerPk psk) psk


-- CHECK: @verifyBlocks
-- Verifies a sequence of blocks.
-- #verifyBlock

-- | Verify a sequence of blocks.
--
-- foldl' is used here which eliminates laziness of triple. It doesn't affect
-- laziness of 'VerificationRes' which is good because laziness for this data
-- type is crucial.
verifyBlocks
    :: forall ssc f t.
       ( SscHelpersClass ssc
       , BiSsc ssc
       , t ~ OldestFirst f (Block ssc)
       , NontrivialContainer t
       )
    => Maybe SlotId
    -> Bool
    -> BlockVersionData
    -> SlotLeaders
    -> ProxySKHeavyMap
    -> OldestFirst f (Block ssc)
    -> VerificationRes
verifyBlocks curSlotId verifyNoUnknown bvd initLeaders initPsks = view _4 . foldl' step start
  where
    start :: VerifyBlocksIter ssc
    -- Note that here we never know previous header before this
    -- function is launched.  Which means that we will not do any
    -- checks related to previous header. And it is fine, because we
    -- must do these checks in advance, when we are processing
    -- headers. However, it's a little obscure invariant, so keep it
    -- in mind.
    start = (initLeaders, initPsks, Nothing, mempty)
    step :: VerifyBlocksIter ssc -> Block ssc -> VerifyBlocksIter ssc
    step (leaders, psks, prevHeader, res) blk =
        let newLeaders = case blk of
                Left genesisBlock -> genesisBlock ^. genBlockLeaders
                Right _           -> leaders
            newPsks = case blk of
                Left _  -> psks
                Right b -> pskHeavyMapApplyBlock b psks
            vhp =
                VerifyHeaderParams
                { vhpPrevHeader = prevHeader
                , vhpLeaders = Just newLeaders
                , vhpCurrentSlot = curSlotId
                , vhpHeavyCerts = Just psks
                , vhpMaxSize = Just (bvdMaxHeaderSize bvd)
                , vhpVerifyNoUnknown = verifyNoUnknown
                }
            vbp =
                VerifyBlockParams
                { vbpVerifyHeader = vhp
                , vbpMaxSize = bvdMaxBlockSize bvd
                , vbpVerifyNoUnknown = verifyNoUnknown
                }
        in (newLeaders, newPsks, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)
