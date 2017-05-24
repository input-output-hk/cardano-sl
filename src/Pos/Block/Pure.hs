{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Pure functions related to blocks and headers.

module Pos.Block.Pure
       ( blockDifficultyIncrement
       , headerDifficultyIncrement

       , VerifyBlockParams (..)
       , VerifyHeaderParams (..)
       , verifyBlock
       , verifyBlocks
       , verifyGenericBlock
       , verifyHeader
       , verifyHeaders
       ) where

import           Universum

import           Control.Lens               (ix, (%=))
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (groupBy, partition)
import           Formatting                 (build, int, sformat, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util.Verify       (VerificationRes (..), verifyGeneric)

import           Pos.Binary.Block.Core      ()
import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Core            ()
import           Pos.Binary.Update          ()
import           Pos.Block.Core             (BiSsc, Block, BlockHeader,
                                             BlockSignature (..), MainBlock,
                                             MainToSign (..), gbhConsensus, gebAttributes,
                                             gehAttributes, genBlockLeaders,
                                             getBlockHeader, getBlockHeader,
                                             mainBlockDlgPayload, mainBlockSscPayload,
                                             mainHeaderLeaderKey, mcdDifficulty,
                                             mcdLeaderKey, mcdSignature, mcdSlot,
                                             mebAttributes, mehAttributes)
import           Pos.Core                   (ChainDifficulty, EpochOrSlot,
                                             HasDifficulty (..), HasEpochIndex (..),
                                             HasEpochOrSlot (..), HasHeaderHash (..),
                                             HeaderHash, ProxySKHeavyMap, SlotId (..),
                                             SlotLeaders, addressHash, gbhExtra,
                                             getSlotIndex, headerSlotL, prevBlockL)
import           Pos.Core.Block             (Blockchain (..), GenericBlock (..),
                                             GenericBlockHeader (..), gbBody, gbBodyProof,
                                             gbExtra, gbHeader)
import           Pos.Crypto                 (ProxySecretKey (..), SignTag (..), checkSig,
                                             pdCert, proxyVerify)
import           Pos.Data.Attributes        (Attributes (attrRemain))
import           Pos.Ssc.Class.Helpers      (SscHelpersClass (..))
import           Pos.Update.Core            (BlockVersionData (..))
import           Pos.Util.Chrono            (NewestFirst (..), OldestFirst)
import           Pos.Util.Util              (Some (Some))


-- | Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficultyIncrement :: BlockHeader ssc -> ChainDifficulty
headerDifficultyIncrement (Left _)  = 0
headerDifficultyIncrement (Right _) = 1

-- | Difficulty of the Block, which is determined from header.
blockDifficultyIncrement :: Block ssc -> ChainDifficulty
blockDifficultyIncrement = headerDifficultyIncrement . getBlockHeader

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
        ]
  where
    verifyBlockSignature (BlockSignature sig) =
        checkSig SignMainBlock pk signature sig
    verifyBlockSignature (BlockPSignatureLight proxySig) =
        proxyVerify SignMainBlockLight
            pk
            proxySig
            (\(epochLow, epochHigh) ->
               epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureHeavy proxySig) =
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
      -- ^ Flag to check signatures and slot index bounds. Doesn't
      -- check that heavyweight cert is allowed to do that, use
      -- 'vhpHeavyCerts' instead.
    , vhpPrevHeader      :: !(Maybe (BlockHeader ssc))
      -- ^ Nothing means that block is unknown, not genesis.
    , vhpNextHeader      :: !(Maybe (BlockHeader ssc))
    , vhpCurrentSlot     :: !(Maybe SlotId)
    , vhpLeaders         :: !(Maybe SlotLeaders)
      -- ^ Set of leaders for the epoch related block is from
    , vhpHeavyCerts      :: !(Maybe ProxySKHeavyMap)
      -- ^ Subset of heavy certs delegation map, where keys only
      -- contain public keys of block issuers passed to checking
      -- function.
    , vhpMaxSize         :: !(Maybe Byte)
    , vhpVerifyNoUnknown :: !Bool
      -- ^ Check that header has no unknown attributes.
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
        , vhpHeavyCerts = Nothing
        , vhpMaxSize = Nothing
        , vhpVerifyNoUnknown = False
        }

maybeEmpty :: Monoid m => (a -> m) -> Maybe a -> m
maybeEmpty = maybe mempty

-- CHECK: @verifyHeader
-- | Check some predicates (determined by 'VerifyHeaderParams') about
-- 'BlockHeader'.
-- #verifyConsensusLocal
--
verifyHeader
    :: forall ssc . BiSsc ssc
    => VerifyHeaderParams ssc -> BlockHeader ssc -> VerificationRes
verifyHeader VerifyHeaderParams {..} h =
    verifyConsensus <> verifyGeneric checks
  where
    verifyConsensus = bool mempty (verifyConsensusLocal h) vhpVerifyConsensus
    checks =
        mconcat
            [ maybeEmpty relatedToPrevHeader vhpPrevHeader
            , maybeEmpty relatedToNextHeader vhpNextHeader
            , maybeEmpty relatedToCurrentSlot vhpCurrentSlot
            , maybeEmpty relatedToLeaders vhpLeaders
            , maybeEmpty heavyCertValid vhpHeavyCerts
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
    :: forall ssc. (SscHelpersClass ssc, BiSsc ssc)
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
                    sscVerifyPayload @ssc
                    (Right $ Some (mainBlk ^. gbHeader))
                    (mainBlk ^. mainBlockSscPayload)
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
                proxySKs = mainBlk ^. mainBlockDlgPayload
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

-- Type alias for common type used inside 'verifyBlocks'
type VerifyBlocksIter ssc =
    (Maybe SlotLeaders, Maybe ProxySKHeavyMap, Maybe (BlockHeader ssc), VerificationRes)

-- Applies block certificates to 'ProxySKHeavyMap'. Used in
-- 'verifyBlocks'. It's not the best place for it here, but i can't
-- put it to "Delegation.Logic" because it leads to dependency cycle.
pskHeavyMapApplyBlock :: MainBlock ssc -> ProxySKHeavyMap -> ProxySKHeavyMap
pskHeavyMapApplyBlock block m = flip execState m $ do
    let (toDelete,toReplace) =
            partition (\ProxySecretKey{..} -> pskIssuerPk == pskDelegatePk)
                      (view mainBlockDlgPayload block)
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
    -> Maybe SlotLeaders
    -> Maybe ProxySKHeavyMap
    -> OldestFirst f (Block ssc)
    -> VerificationRes
verifyBlocks curSlotId verifyNoUnknown bvd initLeaders initPsks = view _4 . foldl' step start
  where
    start :: VerifyBlocksIter ssc
    start = (initLeaders, initPsks, Nothing, mempty)
    step :: VerifyBlocksIter ssc -> Block ssc -> VerifyBlocksIter ssc
    step (leaders, psks, prevHeader, res) blk =
        let newLeaders = case blk of
                Left genesisBlock -> Just $ genesisBlock ^. genBlockLeaders
                Right _           -> leaders
            newPsks = case blk of
                Left _  ->         psks
                Right b ->         pskHeavyMapApplyBlock b <$> psks
            vhp =
                VerifyHeaderParams
                { vhpVerifyConsensus = True
                , vhpPrevHeader = prevHeader
                , vhpNextHeader = Nothing
                , vhpLeaders = newLeaders
                , vhpCurrentSlot = curSlotId
                , vhpHeavyCerts = psks
                , vhpMaxSize = Just (bvdMaxHeaderSize bvd)
                , vhpVerifyNoUnknown = verifyNoUnknown
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
        in (newLeaders, newPsks, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)
