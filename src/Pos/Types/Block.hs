{-# LANGUAGE ConstraintKinds     #-}
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

       , genesisHash

       , VerifyBlockParams (..)
       , VerifyHeaderParams (..)
       , verifyBlock
       , verifyBlocks
       , verifyGenericBlock
       -- , verifyGenericHeader
       , verifyHeader
       , verifyHeaders

       , blockSize
       ) where

import           Control.Lens               (ix)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Default               (Default (def))
import           Data.List                  (groupBy)
import           Data.Tagged                (untag)
import           Formatting                 (build, int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Verify       (VerificationRes (..), verifyGeneric)
import           Universum

import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Types           ()
import           Pos.Binary.Update          ()
import           Pos.Constants              (epochSlots)
import           Pos.Crypto                 (Hash, SecretKey, checkSig, proxySign,
                                             proxyVerify, pskIssuerPk, sign, toPublic,
                                             unsafeHash)
import           Pos.Merkle                 (mkMerkleTree)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass (..))
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Types.Address          (addressHash)
import           Pos.Types.Core             (ChainDifficulty, EpochIndex, EpochOrSlot,
                                             HasDifficulty (..), HasEpochIndex (..),
                                             HasEpochOrSlot (..), HasHeaderHash (..),
                                             HeaderHash, SlotId (..), SlotId)
import           Pos.Types.Tx               (verifyTxAlone)
-- Unqualified import is used here because of GHC bug (trac 12127).
-- See: https://ghc.haskell.org/trac/ghc/ticket/12127
import           Pos.Types.Types
import           Pos.Update.Core            (UpdatePayload)
import           Pos.Util                   (NewestFirst (..), OldestFirst)

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
       ( HasHeaderHash (BBlockHeader b)
       , Blockchain b
       , BHeaderHash b ~ HeaderHash
       )
    => Maybe (BBlockHeader b)
    -> Body b
    -> (BHeaderHash b -> BodyProof b -> ConsensusData b)
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
    h :: HeaderHash
    h = maybe genesisHash headerHash prevHeader
    proof = mkBodyProof body

-- | Smart constructor for 'GenericBlock'. Uses 'mkGenericBlockHeader'.
mkGenericBlock
    :: forall b.
       ( HasHeaderHash (BBlockHeader b)
       , Blockchain b
       , BHeaderHash b ~ HeaderHash
       )
    => Maybe (BBlockHeader b)
    -> Body b
    -> (BHeaderHash b -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> GenericBlock b
mkGenericBlock prevHeader _gbBody consensus extraH _gbExtra = GenericBlock{..}
  where
    _gbHeader = mkGenericHeader prevHeader _gbBody consensus extraH

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: BiSsc ssc
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Maybe ProxySKEither
    -> Body (MainBlockchain ssc)
    -> MainExtraHeaderData
    -> MainBlockHeader ssc
mkMainHeader prevHeader slotId sk pSk body extra =
    mkGenericHeader prevHeader body consensus extra
  where
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign (Left psk)  = BlockPSignatureEpoch $ proxySign sk psk toSign
    makeSignature toSign (Right psk) = BlockPSignatureSimple $ proxySign sk psk toSign
    signature prevHash proof =
        let toSign = (prevHash, proof, slotId, difficulty)
        in maybe (BlockSignature $ sign sk toSign) (makeSignature toSign) pSk
    consensus prevHash proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature prevHash proof
        }

-- | Smart constructor for 'MainBlock'. Uses 'mkMainHeader'.
mkMainBlock
    :: BiSsc ssc
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Maybe ProxySKEither
    -> Body (MainBlockchain ssc)
    -> MainExtraHeaderData
    -> MainExtraBodyData
    -> MainBlock ssc
mkMainBlock prevHeader slotId sk proxyInfo body extraH extraB =
    GenericBlock
    { _gbHeader = mkMainHeader prevHeader slotId sk proxyInfo body extraH
    , _gbBody = body
    , _gbExtra = extraB
    }

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: BiSsc ssc
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
    :: BiSsc ssc
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
mkMainBody
    :: [(Tx, TxWitness, TxDistribution)]
    -> SscPayload ssc
    -> [ProxySKSimple]
    -> UpdatePayload
    -> Body (MainBlockchain ssc)
mkMainBody txws mpc proxySKs updatePayload =
    MainBody
    { _mbTxs                 = mkMerkleTree (map (^. _1) txws)
    , _mbWitnesses           = map (^. _2) txws
    , _mbTxAddrDistributions = map (^. _3) txws
    , _mbMpc                 = mpc
    , _mbProxySKs            = proxySKs
    , _mbUpdatePayload       = updatePayload
    }

-- CHECK: @verifyConsensusLocal
-- Verifies block signature (also proxy) and that slot id is in the correct range.
verifyConsensusLocal
    :: BiSsc ssc
    => BlockHeader ssc -> VerificationRes
verifyConsensusLocal (Left _)       = mempty
verifyConsensusLocal (Right header) =
    verifyGeneric
        [ ( verifyBlockSignature $ consensus ^. mcdSignature
          , "can't verify signature")
        , (siSlot slotId < epochSlots, "slot index is not less than epochSlots")
        ]
  where
    verifyBlockSignature (BlockSignature sig) =
        checkSig pk (_gbhPrevBlock, _gbhBodyProof, slotId, d) sig
    verifyBlockSignature (BlockPSignatureEpoch proxySig) =
        proxyVerify
            pk
            proxySig
            (\(epochLow, epochHigh) ->
               epochId <= epochHigh && epochId >= epochLow)
            (_gbhPrevBlock, _gbhBodyProof, slotId, d)
    verifyBlockSignature (BlockPSignatureSimple proxySig) =
        proxyVerify
            pk
            proxySig
            (const True)
            (_gbhPrevBlock, _gbhBodyProof, slotId, d)
    GenericBlockHeader {_gbhConsensus = consensus
                       ,..} = header
    pk = consensus ^. mcdLeaderKey
    slotId = consensus ^. mcdSlot
    epochId = siEpoch slotId
    d = consensus ^. mcdDifficulty

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderParams ssc = VerifyHeaderParams
    { vhpVerifyConsensus :: !Bool
    , vhpPrevHeader      :: !(Maybe (BlockHeader ssc))
    -- ^ Nothing means that block is unknown, not genesis.
    , vhpNextHeader      :: !(Maybe (BlockHeader ssc))
    , vhpCurrentSlot     :: !(Maybe SlotId)
    , vhpLeaders         :: !(Maybe SlotLeaders)
    } deriving (Show, Eq)

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

-- CHECK: @verifyHeader
-- | Check some predicates (determined by VerifyHeaderParams) about
-- BlockHeader.
-- #verifyConsensusLocal
--
verifyHeader
    :: forall ssc . BiSsc ssc
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
    checkSlot :: EpochOrSlot
              -> EpochOrSlot
              -> (Bool, Text)
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

    -- CHECK: Performs checks related to the previous header:
    --
    --   * Difficulty is correct.
    --   * Hash is correct.
    --   * Epoch/slot are consistent.
    relatedToPrevHeader prevHeader =
        [ checkDifficulty
              (prevHeader ^. difficultyL + headerDifficulty h)
              (h ^. difficultyL)
        , checkHash
              (headerHash prevHeader)
              (h ^. prevBlockL)
        , checkSlot (getEpochOrSlot prevHeader) (getEpochOrSlot h)
        , case h of
              Left  _ -> (True, "") -- check that epochId prevHeader < epochId h performed above
              Right _ -> sameEpoch (prevHeader ^. epochIndexL) (h ^. epochIndexL)
        ]

    -- CHECK: Performs checks related to the next header:
    --
    --  * Difficulty is correct.
    --  * Hash is correct.
    --  * Epoch/slot are consistent.
    relatedToNextHeader nextHeader =
        [ checkDifficulty
              (nextHeader ^. difficultyL - headerDifficulty nextHeader)
              (h ^. difficultyL)
        , checkHash (headerHash h) (nextHeader ^. prevBlockL)
        , checkSlot (getEpochOrSlot h) (getEpochOrSlot nextHeader)
        , case nextHeader of
              Left  _ -> (True, "") -- check that epochId h  < epochId nextHeader performed above
              Right _ -> sameEpoch (h ^. epochIndexL) (nextHeader ^. epochIndexL)
        ]

    -- CHECK: Verifies that the slot does not lie in the future.
    relatedToCurrentSlot curSlotId =
        [ ( either (const True) ((<= curSlotId) . view headerSlot) h
          , "block is from slot which hasn't happened yet")
        ]

    -- CHECK: Checks that the block leader is the expected one.
    relatedToLeaders leaders =
        case h of
            Left _ -> []
            Right mainHeader ->
                [ ( (Just (addressHash $ mainHeader ^. headerLeaderKey) ==
                     leaders ^?
                     ix (fromIntegral $ siSlot $ mainHeader ^. headerSlot))
                  , "block's leader is different from expected one")
                ]

-- | Verifies a set of block headers.
verifyHeaders
    :: BiSsc ssc
    => Bool -> NewestFirst [] (BlockHeader ssc) -> VerificationRes
verifyHeaders _ (NewestFirst []) = mempty
verifyHeaders checkConsensus (NewestFirst (headers@(_:xh))) =
    mconcat verified
  where
    verified = zipWith (\cur prev -> verifyHeader (toVHP prev) cur)
                       headers (map Just xh ++ [Nothing])
    toVHP p = def { vhpVerifyConsensus = checkConsensus
                  , vhpPrevHeader = p }

-- CHECK: @verifyGenericBlock
-- | Perform cheap checks of GenericBlock, which can be done using
-- only block itself. Checks which can be done using only header are
-- ignored here. It is assumed that they will be done separately.
verifyGenericBlock :: forall b . Blockchain b => GenericBlock b -> VerificationRes
verifyGenericBlock blk =
    verifyGeneric
        [ ( checkBodyProof (blk ^. gbBody) (blk ^. gbBodyProof)
          , "body proof doesn't prove body")
        ]

-- | Parameters of Block static verification.
-- Note: to check that block references previous block and/or is referenced
-- by next block, use header verification (via vbpVerifyHeader).
data VerifyBlockParams ssc = VerifyBlockParams
    { vbpVerifyHeader   :: !(Maybe (VerifyHeaderParams ssc))
      -- ^ Verifies header accordingly to params ('verifyHeader')
    , vbpVerifyGeneric  :: !Bool
      -- ^ Checks 'verifyGenesisBlock' property.
    , vbpVerifyTxs      :: !Bool
      -- ^ Checks that each transaction passes 'verifyTxAlone' check.
    , vbpVerifySsc      :: !Bool
      -- ^ Verifies ssc payload with 'sscVerifyPayload'.
    , vbpVerifyProxySKs :: !Bool
      -- ^ Check that's number of sks is limited (1000 for now).
    }

-- | By default nothing is checked.
instance Default (VerifyBlockParams ssc) where
    def =
        VerifyBlockParams
        { vbpVerifyHeader = Nothing
        , vbpVerifyGeneric = False
        , vbpVerifyTxs = False
        , vbpVerifySsc = False
        , vbpVerifyProxySKs = False
        }

-- CHECK: @verifyBlock
-- | Check predicates defined by VerifyBlockParams.
-- #verifyHeader
-- #verifyGenericBlock
verifyBlock
    :: (SscHelpersClass ssc, BiSsc ssc)
    => VerifyBlockParams ssc -> Block ssc -> VerificationRes
verifyBlock VerifyBlockParams {..} blk =
    mconcat
        [ verifyG
        , maybeEmpty (flip verifyHeader (getBlockHeader blk)) vbpVerifyHeader
        , verifyTxs
        , verifySsc
        , verifyProxySKs
        ]
  where
    verifyG
        | vbpVerifyGeneric = either verifyGenericBlock verifyGenericBlock blk
        | otherwise = mempty
    verifyTxs
        | vbpVerifyTxs =
            case blk of
                Left _        -> mempty
                Right mainBlk -> foldMap verifyTxAlone $ mainBlk ^. blockTxs
        | otherwise = mempty
    verifySsc
        | vbpVerifySsc =
            case blk of
                Left _ -> mempty
                Right mainBlk ->
                    untag
                        sscVerifyPayload
                        (mainBlk ^. gbHeader)
                        (mainBlk ^. blockMpc)
        | otherwise = mempty
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $
        sortOn pskIssuerPk psks
    verifyProxySKs
        | vbpVerifyProxySKs =
          (flip (either $ const mempty) blk) $ \mainBlk ->
            let proxySKs = mainBlk ^. blockProxySKs
                duplicates = proxySKsDups proxySKs in
            verifyGeneric
            [ ( null duplicates
              , "Some of block's PSKs have the same issuer, which is prohibited")
            ]
        | otherwise = mempty

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
    -> Maybe SlotLeaders
    -> OldestFirst f (Block ssc)
    -> VerificationRes
verifyBlocks curSlotId initLeaders = view _3 . foldl' step start
  where
    start :: (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
    start = (initLeaders, Nothing, mempty)
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
                { vbpVerifyHeader = Just vhp
                , vbpVerifyGeneric = True
                , vbpVerifyTxs = True
                , vbpVerifySsc = True
                , vbpVerifyProxySKs = True
                }
        in (newLeaders, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)

-- | Compute size of 'MainBlock' in bytes.
blockSize :: Ssc ssc => MainBlock ssc -> Byte
blockSize = fromIntegral . BSL.length . Bi.encode
