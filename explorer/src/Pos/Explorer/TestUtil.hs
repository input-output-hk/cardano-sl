{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Explorer.TestUtil
    ( generateValidExplorerMockableMode
    , produceBlocksByBlockNumberAndSlots
    , basicBlockGenericUnsafe
    , basicBlock
    , emptyBlk
    , leftToCounter
    , produceSlotLeaders
    , produceSecretKeys
    , generateValidBlocksSlotsNumber
    ) where

import qualified Prelude
import           Universum

import           Data.Default (def)
import qualified Data.List.NonEmpty as NE
import           Data.Text.Buildable (build)
import           Serokell.Data.Memory.Units (Byte, Gigabyte, convertUnit)
import           Test.QuickCheck (Arbitrary (..), Property, Testable, Gen, counterexample, forAll,
                                  generate, choose, suchThat, property)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

import           Pos.Arbitrary.Block ()
import           Pos.Block.Base (mkGenesisBlock)
import           Pos.Block.Logic (RawPayload (..), createMainBlockPure)
import           Pos.Block.Types (SlogUndo, Undo)
import qualified Pos.Communication ()
import           Pos.Core (BlockCount (..), ChainDifficulty (..), EpochIndex (..), HasConfiguration,
                           LocalSlotIndex (..), SlotId (..), SlotLeaders, StakeholderId,
                           difficultyL)
import           Pos.Core.Block (Block, BlockHeader, GenesisBlock, MainBlock, getBlockHeader)
import           Pos.Core.Ssc (SscPayload)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpdatePayload (..))
import           Pos.Crypto (SecretKey)
import           Pos.Delegation (DlgPayload, DlgUndo, ProxySKBlockInfo)
import           Pos.Explorer.ExtraContext (ExplorerMockableMode (..))
import           Pos.Ssc.Base (defaultSscPayload)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Test.Pos.Util (withDefConfigurations)


----------------------------------------------------------------
-- Arbitrary and Show instances
----------------------------------------------------------------

-- I used the build function since I suspect that it's safe (even in tests).
instance HasConfiguration => Prelude.Show SlogUndo where
    show = show . build

instance Prelude.Show DlgUndo where
    show = show . build

instance HasConfiguration => Prelude.Show Undo where
    show = show . build

instance Arbitrary SlogUndo where
    arbitrary = genericArbitrary

instance HasConfiguration => Arbitrary DlgUndo where
    arbitrary = genericArbitrary

instance HasConfiguration => Arbitrary Undo where
    arbitrary = genericArbitrary

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

    -- TODO(ks): These should be corrected, they should come from
    -- `produceBlocksByBlockNumberAndSlots` and should not be arbitrary.
    -- createPagedHeaderHashesPair
    blockHHs      <- liftIO $ generate $ arbitrary
    blundsFHHs    <- liftIO $ generate $ withDefConfigurations arbitrary

    slotLeaders   <- produceSlotLeaders blocksNumber
    secretKeys    <- produceSecretKeys blocksNumber

    blocks <- withDefConfigurations $
        produceBlocksByBlockNumberAndSlots blocksNumber slotsPerEpoch slotLeaders secretKeys

    let tipBlock = Prelude.last blocks

    pure $ ExplorerMockableMode
        { emmGetTipBlock          = pure tipBlock
        , emmGetPageBlocks        = \_ -> pure $ Just blockHHs
        , emmGetBlundFromHH       = \_ -> pure $ Just blundsFHHs
        , emmGetSlotStart         = \_ -> pure $ Just slotStart
        , emmGetLeadersFromEpoch  = \_ -> pure $ Just slotLeaders
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

    let generatedEpochBlocksM :: m [Block]
        generatedEpochBlocksM = concatForM [0..totalEpochs] $ \currentEpoch -> do
            generateGenericEpochBlocks
                Nothing
                slotsNumber
                (EpochIndex . fromIntegral $ currentEpoch)

    generatedEpochBlocksM
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



