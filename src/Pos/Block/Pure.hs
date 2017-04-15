{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Pure functions related to blocks and headers.

module Pos.Block.Pure
       ( blockDifficultyIncrement
       , headerDifficultyIncrement
       , mkGenericBlock
       , mkGenericHeader
       , mkMainBlock
       , recreateMainBlock
       , mkMainHeader
       , mkGenesisHeader
       , mkGenesisBlock

       , genesisHash

       , VerifyBlockParams (..)
       , VerifyHeaderParams (..)
       , verifyBlock
       , verifyBlocks
       , verifyGenericBlock
       , verifyHeader
       , verifyHeaders
       ) where

import           Control.Lens               (ix)
import           Data.Default               (Default (def))
import           Data.List                  (groupBy)
import           Data.Tagged                (untag)
import           Formatting                 (build, int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util.Verify       (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Binary.Block.Types     ()
import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Core            ()
import           Pos.Binary.Update          ()
import           Pos.Constants              (epochSlots)
import           Pos.Core                   (ChainDifficulty, EpochIndex, EpochOrSlot,
                                             HasDifficulty (..), HasEpochIndex (..),
                                             HasEpochOrSlot (..), HasHeaderHash (..),
                                             HeaderHash, ProxySKEither, SlotId (..),
                                             SlotLeaders, addressHash, prevBlockL)
import           Pos.Core.Block             (Blockchain (..), GenericBlock (..),
                                             GenericBlockHeader (..), gbBody, gbBodyProof,
                                             gbExtra, gbHeader)
import           Pos.Crypto                 (Hash, SecretKey, SignTag (..), checkSig,
                                             proxySign, proxyVerify, pskIssuerPk,
                                             pskOmega, sign, toPublic, unsafeHash)
import           Pos.Data.Attributes        (Attributes (attrRemain))
import           Pos.Data.Attributes        (mkAttributes)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass (..))
import           Pos.Types.Block.Instances  (Body (..), ConsensusData (..), blockLeaders,
                                             blockMpc, blockProxySKs, getBlockHeader,
                                             getBlockHeader, headerLeaderKey, headerSlot,
                                             mcdDifficulty, mcdLeaderKey, mcdSignature,
                                             mcdSlot)
import           Pos.Types.Block.Types      (BiSsc, Block, BlockHeader,
                                             BlockSignature (..), GenesisBlock,
                                             GenesisBlockHeader, GenesisBlockchain,
                                             GenesisExtraBodyData (..),
                                             GenesisExtraHeaderData (..), MainBlock,
                                             MainBlockHeader, MainBlockchain,
                                             MainExtraBodyData (..), MainExtraHeaderData,
                                             MainToSign (..), gebAttributes,
                                             mebAttributes)
import           Pos.Update.Core            (BlockVersionData (..))
import           Pos.Util.Chrono            (NewestFirst (..), OldestFirst)

-- | Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficultyIncrement :: BlockHeader ssc -> ChainDifficulty
headerDifficultyIncrement (Left _)  = 0
headerDifficultyIncrement (Right _) = 1

-- | Difficulty of the Block, which is determined from header.
blockDifficultyIncrement :: Block ssc -> ChainDifficulty
blockDifficultyIncrement = headerDifficultyIncrement . getBlockHeader

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
    :: (BiSsc ssc, SscHelpersClass ssc)
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
    makeSignature toSign (Left psk) = BlockPSignatureEpoch $
        proxySign SignMainBlockLight sk psk toSign
    makeSignature toSign (Right psk) = BlockPSignatureSimple $
        proxySign SignMainBlockHeavy sk psk toSign
    signature prevHash proof =
        let toSign = MainToSign prevHash proof slotId difficulty extra
        in maybe (BlockSignature $ sign SignMainBlock sk toSign)
                 (makeSignature toSign)
             pSk
    consensus prevHash proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature prevHash proof
        }

-- | Smart constructor for 'MainBlock'. Uses 'mkMainHeader'.
mkMainBlock
    :: (BiSsc ssc, SscHelpersClass ssc, MonadFail m)
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Maybe ProxySKEither
    -> Body (MainBlockchain ssc)
    -> MainExtraHeaderData
    -> MainExtraBodyData
    -> m (MainBlock ssc)
mkMainBlock prevHeader slotId sk proxyInfo body extraH extraB =
    recreateMainBlock
        (mkMainHeader prevHeader slotId sk proxyInfo body extraH)
        body
        extraB

recreateMainBlock
    :: (BiSsc ssc, SscHelpersClass ssc, MonadFail m)
    => MainBlockHeader ssc
    -> Body (MainBlockchain ssc)
    -> MainExtraBodyData
    -> m (MainBlock ssc)
recreateMainBlock _gbHeader _gbBody _gbExtra = do
    let gb = GenericBlock{..}
    case verifyBBlock gb of
        Right _  -> pass
        Left err -> fail $ toString err
    pure gb

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: BiSsc ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> Body (GenesisBlockchain ssc)
    -> GenesisBlockHeader ssc
mkGenesisHeader prevHeader epoch body =
    mkGenericHeader prevHeader body consensus (GenesisExtraHeaderData $ mkAttributes ())
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
    , _gbExtra = GenesisExtraBodyData $ mkAttributes ()
    }
  where
    body = GenesisBody leaders

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
        checkSig SignMainBlock pk signature sig
    verifyBlockSignature (BlockPSignatureEpoch proxySig) =
        proxyVerify SignMainBlockLight
            pk
            proxySig
            (\(epochLow, epochHigh) ->
               epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureSimple proxySig) =
        proxyVerify SignMainBlockHeavy
            pk
            proxySig
            (const True)
            signature
    GenericBlockHeader { _gbhConsensus = consensus
                       , _gbhExtra = extra
                       , ..} = header
    signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId d extra
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
    , vhpMaxSize         :: !(Maybe Byte)
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
        , vhpMaxSize = Nothing
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
            , checkSize
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

    -- CHECK: Performs checks related to the next header:
    --
    --  * Difficulty is correct.
    --  * Hash is correct.
    --  * Epoch/slot are consistent.
    relatedToNextHeader nextHeader =
        [ checkDifficulty
              (nextHeader ^. difficultyL - headerDifficultyIncrement nextHeader)
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
verifyHeaders checkConsensus (NewestFirst (headers@(_:xh))) = mconcat verified
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
    { vbpVerifyHeader    :: !(Maybe (VerifyHeaderParams ssc))
      -- ^ Verifies header accordingly to params ('verifyHeader')
    , vbpVerifyGeneric   :: !Bool
      -- ^ Checks 'verifyGenesisBlock' property.
    , vbpVerifySsc       :: !Bool
      -- ^ Verifies ssc payload with 'sscVerifyPayload'.
    , vbpVerifyProxySKs  :: !Bool
      -- ^ Check that's number of sks is limited (1000 for now).
    , vbpMaxSize         :: !(Maybe Byte)
    -- ^ Maximal block size.
    , vbpVerifyNoUnknown :: !Bool
    -- ^ Check that block has no unknown attributes.
    }

-- TODO: get rid of this module
-- | By default nothing is checked.
instance Default (VerifyBlockParams ssc) where
    def =
        VerifyBlockParams
        { vbpVerifyHeader = Nothing
        , vbpVerifyGeneric = False
        , vbpVerifySsc = False
        , vbpVerifyProxySKs = False
        , vbpMaxSize =  Nothing
        , vbpVerifyNoUnknown = False
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
        , verifySsc
        , verifyProxySKs
        , maybeEmpty checkSize vbpMaxSize
        , bool mempty (verifyNoUnknown blk) vbpVerifyNoUnknown
        ]
  where
    toVerRes (Right _) = VerSuccess
    toVerRes (Left e)  = VerFailure [sformat build e]

    verifyG
        | vbpVerifyGeneric = either verifyGenericBlock verifyGenericBlock blk
        | otherwise = mempty
    verifySsc
        | vbpVerifySsc =
            case blk of
                Left _ -> mempty
                Right mainBlk -> toVerRes $
                    untag
                        sscVerifyPayload
                        (Right (mainBlk ^. gbHeader))
                        (mainBlk ^. blockMpc)
        | otherwise = mempty
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $
        sortOn pskIssuerPk psks
    verifyProxySKs
        | vbpVerifyProxySKs =
          (flip (either $ const mempty) blk) $ \mainBlk ->
            let bEpoch = mainBlk ^. epochIndexL
                notMatchingEpochs = filter ((/= bEpoch) . pskOmega) proxySKs
                proxySKs = mainBlk ^. blockProxySKs
                duplicates = proxySKsDups proxySKs in
                verifyGeneric
            [ ( null duplicates
              , "Some of block's PSKs have the same issuer, which is prohibited"),
              ( null notMatchingEpochs
              , "Block contains psk(s) that have non-matching epoch index")
            ]
        | otherwise = mempty
    checkSize maxSize = verifyGeneric [
      (Bi.biSize blk <= maxSize,
       sformat ("block's size exceeds limit ("%memory%" > "%memory%")")
       (Bi.biSize blk) maxSize)
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
    -> Maybe SlotLeaders
    -> OldestFirst f (Block ssc)
    -> VerificationRes
verifyBlocks curSlotId verifyNoUnknown bvd initLeaders = view _3 . foldl' step start
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
                , vhpMaxSize = Just (bvdMaxHeaderSize bvd)
                }
            vbp =
                VerifyBlockParams
                { vbpVerifyHeader = Just vhp
                , vbpVerifyGeneric = True
                , vbpVerifySsc = True
                , vbpVerifyProxySKs = True
                , vbpMaxSize = Just (bvdMaxBlockSize bvd)
                , vbpVerifyNoUnknown = verifyNoUnknown
                }
        in (newLeaders, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)
