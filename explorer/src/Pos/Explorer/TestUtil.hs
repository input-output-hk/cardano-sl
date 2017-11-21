{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Explorer.TestUtil
    ( BlockNumber
    , SlotsPerEpoch
    , generateValidExplorerMockableMode
    , produceBlocksByBlockNumberAndSlots
    , basicBlockGenericUnsafe
    , basicBlock
    , emptyBlk
    , leftToCounter
    , produceSlotLeaders
    , produceSecretKeys
    , generateValidBlocksSlotsNumber
    , createEmptyUndo
    ) where

import qualified Prelude
import           Universum

import           Control.Lens (at)
import           Data.Default (def)
import           Data.Map (fromList, fromListWith)
import qualified Data.List.NonEmpty as NE
import           Serokell.Data.Memory.Units (Byte, Gigabyte, convertUnit)
import           Test.QuickCheck (Arbitrary (..), Property, Testable, Gen, counterexample, forAll,
                                  generate, choose, suchThat, property)

import           Pos.Arbitrary.Block ()
import           Pos.Block.Base (mkGenesisBlock)
import           Pos.Block.Logic (RawPayload (..), createMainBlockPure)
import           Pos.Block.Types (Blund, Undo (..), SlogUndo (..))
import qualified Pos.Communication ()
import           Pos.Core (BlockCount (..), ChainDifficulty (..), EpochIndex (..), HasConfiguration,
                           LocalSlotIndex (..), SlotId (..), SlotLeaders, StakeholderId, HeaderHash,
                           difficultyL, headerHash)
import           Pos.Core.Block (Block, BlockHeader, GenesisBlock, MainBlock, getBlockHeader)
import           Pos.Core.Ssc (SscPayload)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpdatePayload (..))
import           Pos.Crypto (SecretKey)
import           Pos.Delegation (DlgPayload, DlgUndo (..), ProxySKBlockInfo)
import           Pos.Ssc.Base (defaultSscPayload)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Test.Pos.Util (withDefConfigurations)

import           Pos.Explorer.ExtraContext (ExplorerMockableMode (..))
import           Pos.Explorer.DB (Page)
import           Pos.Explorer.BListener (createPagedHeaderHashesPair)


----------------------------------------------------------------
-- Util types
----------------------------------------------------------------

-- TODO(ks): We can migrate to newtype(s) at some point, good enough for now.
type BlockNumber       = Word
type SlotsPerEpoch     = Word
type TotalEpochs       = Word
type SlotLeadersNumber = Word

----------------------------------------------------------------
-- Function mocks
----------------------------------------------------------------

-- | More predictable generation that doesn't violate the invariants.
generateValidExplorerMockableMode
    :: BlockNumber
    -> SlotsPerEpoch
    -> IO ExplorerMockableMode
generateValidExplorerMockableMode blocksNumber slotsPerEpoch = do

    slotStart     <- liftIO $ generate $ arbitrary

    slotLeaders   <- produceSlotLeaders blocksNumber
    secretKeys    <- produceSecretKeys blocksNumber

    blocks <- withDefConfigurations $
        produceBlocksByBlockNumberAndSlots blocksNumber slotsPerEpoch slotLeaders secretKeys

    let tipBlock  = Prelude.last blocks
    let pagedHHs  = withDefConfigurations $ createMapPageHHs blocks
    let hHsBlunds = withDefConfigurations $ createMapHHsBlund blocks

    pure $ ExplorerMockableMode
        { emmGetTipBlock          = pure tipBlock
        , emmGetPageBlocks        = \page -> pure $ pagedHHs ^. at page
        , emmGetBlundFromHH       = \hh   -> pure $ hHsBlunds ^. at hh
        , emmGetSlotStart         = \_    -> pure $ Just slotStart
        , emmGetLeadersFromEpoch  = \_    -> pure $ Just slotLeaders
        }
  where
    createMapPageHHs :: (HasConfiguration) => [Block] -> Map Page [HeaderHash]
    createMapPageHHs blocks =
        fromListWith (++) [ (page, [hHash]) | (page, hHash) <- createPagedHeaderHashesPair blocks]

    createMapHHsBlund :: (HasConfiguration) => [Block] -> Map HeaderHash Blund
    createMapHHsBlund blocks = fromList $ map blockHH blocks
      where
        blockHH :: Block -> (HeaderHash, Blund)
        blockHH block = (headerHash block, (block, createEmptyUndo))

-- | The first aproximation. Ideally, I wanted to have something to generate @Undo@ from
-- @Block@. We could generate @Undo@ from _produceBlocksByBlockNumberAndSlots_, but we
-- need to add quite a bit of logic to it. For now, it's good enough, since we are
-- testing just the "sunny day" scenario, and we don't worry about rollbacks. There
-- are already tests that cover a lot of rollback logic.
-- For a more realistic @Undo@, check @Pos.Block.Logic.VAR.verifyBlocksPrefix@.
createEmptyUndo :: Undo
createEmptyUndo = Undo
    { undoTx = mempty
    , undoDlg = DlgUndo mempty mempty
    , undoUS = def
    , undoSlog = SlogUndo Nothing
    }

-- | Simplify the generation of blocks number and slots.
-- For now we have a minimum of one epoch.
generateValidBlocksSlotsNumber :: Gen (Word, Word)
generateValidBlocksSlotsNumber = do
    totalBlocksNumber <- choose (3, 1000)
    -- You can have minimally two blocks - genesis + main block
    slotsPerEpoch     <- choose (2, 1000) `suchThat` (< totalBlocksNumber)
    pure (totalBlocksNumber, slotsPerEpoch)

----------------------------------------------------------------
-- Utility
-- TODO(ks): Extract this in some common PureBlockTest module in lib/?
----------------------------------------------------------------

basicBlockGenericUnsafe
    :: (HasConfiguration, HasUpdateConfiguration)
    => BlockHeader
    -> SecretKey
    -> SlotId
    -> Block
basicBlockGenericUnsafe prevHeader sk slotId = case (basicBlock prevHeader sk slotId) of
    Left e      -> error e
    Right block -> Right block

basicBlock
    :: (HasConfiguration, HasUpdateConfiguration)
    => BlockHeader
    -> SecretKey
    -> SlotId
    -> Either Text MainBlock
basicBlock prevHeader sk slotId =
    producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk

emptyBlk
    :: (HasConfiguration, HasUpdateConfiguration, Testable p)
    => (Either Text MainBlock -> p)
    -> Property
emptyBlk testableBlock =
    forAll arbitrary $ \(sk, prevHeader, slotId) ->
    testableBlock
        $ producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk

defGTP :: HasConfiguration => SlotId -> SscPayload
defGTP sId = defaultSscPayload $ siSlot sId

infLimit :: Byte
infLimit = convertUnit @Gigabyte @Byte 1

producePureBlock
    :: (HasConfiguration, HasUpdateConfiguration)
    => Byte
    -> BlockHeader
    -> [TxAux]
    -> ProxySKBlockInfo
    -> SlotId
    -> DlgPayload
    -> SscPayload
    -> UpdatePayload
    -> SecretKey
    -> Either Text MainBlock
producePureBlock limit prev txs psk slot dlgPay sscPay usPay sk =
    createMainBlockPure limit prev psk slot sk $
    RawPayload txs sscPay dlgPay usPay

leftToCounter :: (ToString s, Testable p) => Either s a -> (a -> p) -> Property
leftToCounter x c = either (\t -> counterexample (toString t) False) (property . c) x


-- | Function that should generate arbitrary blocks that we can use in tests.
produceBlocksByBlockNumberAndSlots
    :: forall m. (HasConfiguration, HasUpdateConfiguration, MonadIO m, Monad m)
    => BlockNumber
    -> SlotsPerEpoch
    -> SlotLeaders
    -> [SecretKey]
    -> m [Block]
produceBlocksByBlockNumberAndSlots blockNumber slotsNumber producedSlotLeaders secretKeys = do

    -- This is just plain wrong and we need to check for it.
    when (blockNumber < slotsNumber) $ error "Illegal argument."

    concatForM [0..totalEpochs] $ \currentEpoch -> do
        generateGenericEpochBlocks
            Nothing
            slotsNumber
            (EpochIndex . fromIntegral $ currentEpoch)
  where

    totalEpochs :: TotalEpochs
    totalEpochs = blockNumber `div` slotsNumber

    remainingSlots :: SlotsPerEpoch
    remainingSlots = blockNumber `mod` slotsNumber

    generateGenericEpochBlocks
        :: Maybe BlockHeader
        -> SlotsPerEpoch
        -> EpochIndex
        -> m [Block]
    generateGenericEpochBlocks mBlockHeader slotsPerEpoch epochIndex = do

        let generatedBlocks = generateEpochBlocks mBlockHeader slotsPerEpoch epochIndex

        let gbToMainBlock :: Block
            gbToMainBlock = Left . fst $ generatedBlocks

        let mainBlocks :: [MainBlock]
            mainBlocks = snd generatedBlocks

        let mbToMainBlock :: [Block]
            mbToMainBlock = Right <$> mainBlocks

        pure $ [gbToMainBlock] ++ mbToMainBlock

    generateEpochBlocks
        :: Maybe BlockHeader
        -> SlotsPerEpoch
        -> EpochIndex
        -> (GenesisBlock, [MainBlock])
    generateEpochBlocks mBlockHeader slotsPerEpoch' epochIndex =
        (epochGenesisBlock, epochBlocks)
      where
        epochGenesisBlock :: GenesisBlock
        epochGenesisBlock = mkGenesisBlock mBlockHeader epochIndex producedSlotLeaders

        epochBlocks :: [MainBlock]
        epochBlocks = do
            -- TODO(ks): Need to create prev. blocks that are not dependant on genesis block.
            let mainBlocks = foldl' epochBlocksCalculation (getPrevBlockHeader, []) totalBlockNumbers

            mainBlocks ^. _2
          where
            epochBlocksCalculation
                :: (BlockHeader, [MainBlock])
                -> Word
                -> (BlockHeader, [MainBlock])
            epochBlocksCalculation (previousBlockHeader, mainBlocks) currentBlockNumber = do

                let newBlock :: MainBlock
                    newBlock = generateBlocks previousBlockHeader currentBlockNumber

                let newBlockHeader :: BlockHeader
                    newBlockHeader = getBlockHeader . Right $ newBlock

                let newMainBlocks :: [MainBlock]
                    newMainBlocks = mainBlocks ++ [newBlock]

                (newBlockHeader, newMainBlocks)

            totalBlockNumbers :: [Word]
            totalBlockNumbers = [1..slotsPerEpoch']

            getPrevBlockHeader :: BlockHeader
            getPrevBlockHeader = getBlockHeader . Left $ epochGenesisBlock

            generateBlocks
                :: (HasConfiguration, HasUpdateConfiguration)
                => BlockHeader
                -> BlockNumber
                -> MainBlock
            generateBlocks previousBlockHeader blockNumber' =

                case basicBlock previousBlockHeader currentSecretKey slotId of
                    Left _      -> error "Block creation error!"
                    Right block ->
                        block & difficultyL .~ (ChainDifficulty currentBlockCount)
              where
                -- If we are on the last epoch we don't neccesarily have the full number of
                -- slots.
                currentBlockCount :: BlockCount
                currentBlockCount =
                    if epochIndex == (EpochIndex . fromIntegral $ totalEpochs)
                        then BlockCount $
                              fromIntegral $
                              (remainingSlots + (blockNumber' * epochIndexWord))
                        else BlockCount $
                              fromIntegral $
                              blockNumber' * (epochIndexWord + 1)
                  where
                    -- | TODO(ks): This is wrong, @EpochIndex@ is @Word64@ and the whole
                    -- calculation should be corrected because of a possible overflow.
                    -- But since we aren't using some gigantic @Epoch@ numbers, good for now.
                    epochIndexWord :: Word
                    epochIndexWord = fromIntegral $ getEpochIndex epochIndex

                slotId :: SlotId
                slotId = SlotId
                    { siEpoch = epochIndex
                    , siSlot  = UnsafeLocalSlotIndex integralBlockNumber
                    }

                currentSecretKey :: SecretKey
                currentSecretKey = secretKeys Prelude.!! integralBlockNumber

                integralBlockNumber :: Integral a => a
                integralBlockNumber = fromIntegral blockNumber'


-- | Produce N slot leaders so we can test it realistically.
produceSlotLeaders :: (MonadIO m, Monad m) => SlotLeadersNumber -> m SlotLeaders
produceSlotLeaders slotLeadersNumber = liftIO $ NE.fromList <$> stakeholders
  where
    stakeholders :: IO [StakeholderId]
    stakeholders = replicateM (fromIntegral slotLeadersNumber) generatedStakeHolder
      where
        generatedStakeHolder :: IO StakeholderId
        generatedStakeHolder = generate arbitrary

-- | Produce N secret keys so we can test it realistically.
produceSecretKeys :: (MonadIO m, Monad m) => BlockNumber -> m [SecretKey]
produceSecretKeys blocksNumber = liftIO $ secretKeys
  where
    secretKeys :: IO [SecretKey]
    secretKeys = replicateM (fromIntegral blocksNumber) generatedSecretKey
      where
        generatedSecretKey :: IO SecretKey
        generatedSecretKey = generate arbitrary



