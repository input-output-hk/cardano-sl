{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Functions related to blocks and headers.

module Pos.Types.Block
       ( blockDifficulty
       , headerDifficulty
       , mkGenericBlock
       , mkGenericHeader
       , mkMainBlock
       , mkMainBody
       , mkMainHeader
       , mkGenesisHeader
       , mkGenesisBlock

       , VerifyBlockParams (..)
       , VerifyHeaderParams (..)
       , verifyBlock
       , verifyBlocks
       , verifyGenericBlock
       -- , verifyGenericHeader
       , verifyHeader
       ) where

import           Control.Lens         (ix, view, (^.), (^?), _3)
import           Data.Binary          (Binary)
import           Data.Default         (Default (def))
import           Formatting           (build, int, sformat, (%))
import           Serokell.Util.Verify (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Constants        (epochSlots)
import           Pos.Crypto           (Hash, SecretKey, checkSig, hash, sign, toPublic,
                                       unsafeHash)
import           Pos.Merkle           (mkMerkleTree)
import           Pos.Ssc.Class.Types  (Ssc (..))
-- Unqualified import is used here because of GHC bug (trac 12127).
-- See: https://ghc.haskell.org/trac/ghc/ticket/12127
import           Pos.Types.Types

-- | Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficulty :: BlockHeader ssc -> ChainDifficulty
headerDifficulty (Left _)  = 0
headerDifficulty (Right _) = 1

-- | Difficulty of the Block, which is determined from header.
blockDifficulty :: Block ssc -> ChainDifficulty
blockDifficulty = headerDifficulty . getBlockHeader

-- | Predefined 'Hash' of 'GenesisBlock'.
genesisHash :: Hash a
genesisHash = unsafeHash ("patak" :: Text)
{-# INLINE genesisHash #-}

-- | Smart constructor for 'GenericBlockHeader'.
mkGenericHeader
    :: forall b.
       (Binary (BBlockHeader b), Blockchain b)
    => Maybe (BBlockHeader b)
    -> Body b
    -> (Hash (BBlockHeader b) -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> GenericBlockHeader b
mkGenericHeader prevHeader body consensus extra =
    GenericBlockHeader
    { _gbhPrevBlock = h
    , _gbhBodyProof = proof
    , _gbhConsensus = consensus h proof
    , _gbhExtra = extra
    }
  where
    h :: Hash (BBlockHeader b)
    h = maybe genesisHash hash prevHeader
    proof = mkBodyProof body

-- | Smart constructor for 'GenericBlock'. Uses 'mkGenericBlockHeader'.
mkGenericBlock
    :: forall b.
       (Binary (BBlockHeader b), Blockchain b)
    => Maybe (BBlockHeader b)
    -> Body b
    -> (Hash (BBlockHeader b) -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> GenericBlock b
mkGenericBlock prevHeader body consensus extraH extraB =
    GenericBlock {_gbHeader = header, _gbBody = body, _gbExtra = extraB}
  where
    header = mkGenericHeader prevHeader body consensus extraH

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: Ssc ssc
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Body (MainBlockchain ssc)
    -> MainBlockHeader ssc
mkMainHeader prevHeader slotId sk body =
    mkGenericHeader prevHeader body consensus ()
  where
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    signature prevHash proof = sign sk (prevHash, proof, slotId, difficulty)
    consensus prevHash proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = toPublic sk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature prevHash proof
        }

-- | Smart constructor for 'MainBlock'. Uses 'mkMainHeader'.
mkMainBlock
    :: Ssc ssc
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Body (MainBlockchain ssc)
    -> MainBlock ssc
mkMainBlock prevHeader slotId sk body =
    GenericBlock
    { _gbHeader = mkMainHeader prevHeader slotId sk body
    , _gbBody = body
    , _gbExtra = ()
    }

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: Ssc ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> Body (GenesisBlockchain ssc)
    -> GenesisBlockHeader ssc
mkGenesisHeader prevHeader epoch body =
    mkGenericHeader prevHeader body consensus ()
  where
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ =
        GenesisConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty}

-- | Smart constructor for 'GenesisBlock'. Uses 'mkGenesisHeader'.
mkGenesisBlock
    :: Ssc ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock ssc
mkGenesisBlock prevHeader epoch leaders =
    GenericBlock
    { _gbHeader = mkGenesisHeader prevHeader epoch body
    , _gbBody = body
    , _gbExtra = ()
    }
  where
    body = GenesisBody leaders

-- | Smart constructor for 'Body' of 'MainBlockchain'.
mkMainBody :: [Tx] -> SscPayload ssc -> Body (MainBlockchain ssc)
mkMainBody txs mpc = MainBody {_mbTxs = mkMerkleTree txs, _mbMpc = mpc}

verifyConsensusLocal :: Ssc ssc => BlockHeader ssc -> VerificationRes
verifyConsensusLocal (Left _)       = mempty
verifyConsensusLocal (Right header) =
    verifyGeneric
        [ ( checkSig pk (_gbhPrevBlock, _gbhBodyProof, slotId, d) sig
          , "can't verify signature")
        , (siSlot slotId < epochSlots, "slot index is not less than epochSlots")
        ]
  where
    GenericBlockHeader {_gbhConsensus = consensus, ..} = header
    pk = consensus ^. mcdLeaderKey
    slotId = consensus ^. mcdSlot
    d = consensus ^. mcdDifficulty
    sig = consensus ^. mcdSignature

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderParams ssc = VerifyHeaderParams
    { vhpVerifyConsensus :: !Bool
    , vhpPrevHeader      :: !(Maybe (BlockHeader ssc))
    -- ^ Nothing means that block is unknown, not genesis.
    , vhpNextHeader      :: !(Maybe (BlockHeader ssc))
    , vhpCurrentSlot     :: !(Maybe SlotId)
    , vhpLeaders         :: !(Maybe SlotLeaders)
    }

-- | By default nothing is checked.
instance Default (VerifyHeaderParams ssc) where
    def =
        VerifyHeaderParams
        { vhpVerifyConsensus = False
        , vhpPrevHeader = Nothing
        , vhpNextHeader = Nothing
        , vhpCurrentSlot = Nothing
        , vhpLeaders = Nothing
        }

maybeEmpty :: Monoid m => (a -> m) -> Maybe a -> m
maybeEmpty = maybe mempty

-- | Check some predicates (determined by VerifyHeaderParams) about
-- BlockHeader.
verifyHeader
    :: Ssc ssc
    => VerifyHeaderParams ssc -> BlockHeader ssc -> VerificationRes
verifyHeader VerifyHeaderParams {..} h =
   consensusRes <> verifyGeneric checks
  where
    consensusRes | vhpVerifyConsensus = verifyConsensusLocal h
                 | otherwise = mempty
    checks =
        mconcat
            [ maybeEmpty relatedToPrevHeader vhpPrevHeader
            , maybeEmpty relatedToNextHeader vhpNextHeader
            , maybeEmpty relatedToCurrentSlot vhpCurrentSlot
            , maybeEmpty relatedToLeaders vhpLeaders
            ]
    checkHash expectedHash actualHash =
        ( expectedHash == actualHash
        , sformat
              ("inconsistent hash (expected " %build % ", found" %build % ")")
              expectedHash
              actualHash)
    checkDifficulty expectedDifficulty actualDifficulty =
        ( expectedDifficulty == actualDifficulty
        , sformat
              ("incorrect difficulty (expected " %int % ", found " %int % ")")
              expectedDifficulty
              actualDifficulty)
    checkSlot :: EpochOrSlot
              -> EpochOrSlot
              -> (Bool, Text)
    checkSlot oldSlot newSlot =
        ( oldSlot < newSlot
        , sformat
              ("slots are not monotonic (" %build% " > " %build% ")")
              oldSlot newSlot
        )
    sameEpoch oldEpoch newEpoch =
        ( oldEpoch == newEpoch
        , sformat
              ("two adjacent blocks are from different epochs ("%build%" > "%build%")")
              oldEpoch newEpoch
        )
    relatedToPrevHeader prevHeader =
        [ checkDifficulty
              (prevHeader ^. difficultyL + headerDifficulty h)
              (h ^. difficultyL)
        , checkHash (hash prevHeader) (h ^. prevBlockL)
        , checkSlot (getEpochOrSlot prevHeader) (getEpochOrSlot h)
        , case h of
              Left  _ -> (True, "") -- check that epochId prevHeader < epochId h performed above
              Right _ -> sameEpoch (prevHeader ^. epochIndexL) (h ^. epochIndexL)
        ]
    relatedToNextHeader nextHeader =
        [ checkDifficulty
              (nextHeader ^. difficultyL - headerDifficulty nextHeader)
              (h ^. difficultyL)
        , checkHash (hash h) (nextHeader ^. prevBlockL)
        , checkSlot (getEpochOrSlot h) (getEpochOrSlot nextHeader)
        ]
    relatedToCurrentSlot curSlotId =
        [ ( either (const True) ((<= curSlotId) . view headerSlot) h
          , "block is from slot which hasn't happened yet")
        ]
    relatedToLeaders leaders =
        case h of
            Left _ -> []
            Right mainHeader ->
                [ ( (Just (makePubKeyAddress $ mainHeader ^. headerLeaderKey) ==
                     leaders ^?
                     ix (fromIntegral $ siSlot $ mainHeader ^. headerSlot))
                  , "block's leader is different from expected one")
                ]

-- | Perform cheap checks of GenericBlock, which can be done using
-- only block itself. Checks which can be done using only header are
-- ignored here. It is assumed that they will be done separately.
verifyGenericBlock :: forall b . Blockchain b => GenericBlock b -> VerificationRes
verifyGenericBlock blk =
    verifyGeneric
        [ ( checkBodyProof (blk ^. gbBody) (blk ^. gbBodyProof)
          , "body proof doesn't prove body")
        ]

-- | Parameters of Block verification.
-- Note: to check that block references previous block and/or is referenced
-- by next block, use header verification (via vbpVerifyHeader).
data VerifyBlockParams ssc = VerifyBlockParams
    { vbpVerifyHeader  :: !(Maybe (VerifyHeaderParams ssc))
    , vbpVerifyGeneric :: !Bool
    }

-- | By default nothing is checked.
instance Default (VerifyBlockParams ssc) where
    def =
        VerifyBlockParams
        { vbpVerifyHeader = Nothing
        , vbpVerifyGeneric = False
        }

-- | Check predicates defined by VerifyBlockParams.
verifyBlock :: Ssc ssc => VerifyBlockParams ssc -> Block ssc -> VerificationRes
verifyBlock VerifyBlockParams {..} blk =
    mconcat
        [ verifyG
        , maybeEmpty (flip verifyHeader (getBlockHeader blk)) vbpVerifyHeader
        ]
  where
    verifyG =
        if vbpVerifyGeneric
            then either verifyGenericBlock verifyGenericBlock blk
            else mempty

-- | Verify sequence of blocks. It is assumed that the leftmost block
-- is the oldest one.
-- foldl' is used here which eliminates laziness of triple.
-- It doesn't affect laziness of 'VerificationRes' which is good
-- because laziness for this data type is crucial.
verifyBlocks
    :: forall ssc t. (Ssc ssc, Foldable t)
    => Maybe SlotId -> t (Block ssc) -> VerificationRes
verifyBlocks curSlotId = (view _3) . foldl' step start
  where
    start :: (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
    start = (Nothing, Nothing, mempty)
    step
        :: (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
        -> Block ssc
        -> (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
    step (leaders, prevHeader, res) blk =
        let newLeaders =
                case blk of
                    Left genesisBlock -> Just $ genesisBlock ^. blockLeaders
                    Right _           -> leaders
            vhp =
                VerifyHeaderParams
                { vhpVerifyConsensus = True
                , vhpPrevHeader = prevHeader
                , vhpNextHeader = Nothing
                , vhpLeaders = newLeaders
                , vhpCurrentSlot = curSlotId
                }
            vbp =
                VerifyBlockParams
                {vbpVerifyHeader = Just vhp, vbpVerifyGeneric = True}
        in (newLeaders, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)
