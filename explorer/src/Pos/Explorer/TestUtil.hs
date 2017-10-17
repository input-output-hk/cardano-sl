{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Explorer.TestUtil where

import qualified Prelude
import           Universum

import           Data.Default                      (def)
import qualified Data.List.NonEmpty                as NE
import           Data.Text.Buildable               (build)
import           Serokell.Data.Memory.Units        (Byte, Gigabyte, convertUnit)
import           Test.QuickCheck                   (Arbitrary (..), Property, Testable,
                                                    counterexample, forAll, generate,
                                                    property)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

import           Pos.Arbitrary.Block               ()
import           Pos.Block.Core                    (Block, BlockHeader, GenesisBlock,
                                                    MainBlock, getBlockHeader)
import           Pos.Block.Core.Genesis.Misc       (mkGenesisBlock)
import           Pos.Block.Logic                   (RawPayload (..), createMainBlockPure)
import           Pos.Block.Types                   (SlogUndo, Undo)
import qualified Pos.Communication                 ()
import           Pos.Core                          (EpochIndex (..), HasConfiguration,
                                                    LocalSlotIndex (..), SlotId (..),
                                                    SlotLeaders, StakeholderId, ChainDifficulty (..), BlockCount (..), difficultyL)
import           Pos.Crypto                        (SecretKey)
import           Pos.Delegation                    (DlgPayload, DlgUndo, ProxySKBlockInfo)
import           Pos.Ssc.Class                     (Ssc (..), sscDefaultPayload)
import           Pos.Ssc.GodTossing                (GtPayload (..), SscGodTossing)
import           Pos.Txp.Core                      (TxAux)
import           Pos.Update.Configuration          (HasUpdateConfiguration)
import           Pos.Update.Core                   (UpdatePayload (..))


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
-- Utility
-- TODO(ks): Extract this in some common PureBlockTest module in src/?
----------------------------------------------------------------

basicBlockGenericUnsafe
    :: (HasConfiguration, HasUpdateConfiguration)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Block SscGodTossing
basicBlockGenericUnsafe prevHeader sk slotId = case (basicBlock prevHeader sk slotId) of
    Left e      -> error e
    Right block -> Right block

basicBlock
    :: (HasConfiguration, HasUpdateConfiguration)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Either Text (MainBlock SscGodTossing)
basicBlock prevHeader sk slotId =
    producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasConfiguration => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit :: Byte
    infLimit = convertUnit @Gigabyte @Byte 1

emptyBlk
    :: (HasConfiguration, HasUpdateConfiguration, Testable p)
    => (Either Text (MainBlock SscGodTossing) -> p)
    -> Property
emptyBlk testableBlock =
    forAll arbitrary $ \(sk, prevHeader, slotId) ->
    testableBlock
        $ producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasConfiguration => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit :: Byte
    infLimit = convertUnit @Gigabyte @Byte 1

producePureBlock
    :: (HasConfiguration, HasUpdateConfiguration)
    => Byte
    -> BlockHeader SscGodTossing
    -> [TxAux]
    -> ProxySKBlockInfo
    -> SlotId
    -> DlgPayload
    -> SscPayload SscGodTossing
    -> UpdatePayload
    -> SecretKey
    -> Either Text (MainBlock SscGodTossing)
producePureBlock limit prev txs psk slot dlgPay sscPay usPay sk =
    createMainBlockPure limit prev psk slot sk $
    RawPayload txs sscPay dlgPay usPay

leftToCounter :: (ToString s, Testable p) => Either s a -> (a -> p) -> Property
leftToCounter x c = either (\t -> counterexample (toString t) False) (property . c) x

type BlockNumber   = Word
type SlotsPerEpoch = Word
type TotalEpochs   = Word

-- | Function that should generate arbitrary blocks that we can use in tests.
produceBlocksByBlockNumberAndSlots
    :: forall m. (HasConfiguration, HasUpdateConfiguration, MonadIO m, Monad m)
    => BlockNumber
    -> SlotsPerEpoch
    -> SlotLeaders
    -> [SecretKey]
    -> m [Block SscGodTossing]
produceBlocksByBlockNumberAndSlots blockNumber slotsNumber producedSlotLeaders secretKeys = do

    -- This is just plain wrong and we need to check for it.
    when (blockNumber < slotsNumber) $ error "Illegal argument."

    let generatedEpochBlocksM :: m [[Block SscGodTossing]]
        generatedEpochBlocksM = forM [0..totalEpochs] $ \currentEpoch -> do
            generateGenericEpochBlocks Nothing slotsNumber (EpochIndex . fromIntegral $ currentEpoch)

    concat <$> generatedEpochBlocksM
  where

    totalEpochs :: TotalEpochs
    totalEpochs = blockNumber `div` slotsNumber

    generateGenericEpochBlocks
        :: Maybe (BlockHeader SscGodTossing)
        -> SlotsPerEpoch
        -> EpochIndex
        -> m [Block SscGodTossing]
    generateGenericEpochBlocks mBlockHeader slotsPerEpoch epochIndex = do
        let generatedBlocks = generateEpochBlocks mBlockHeader slotsPerEpoch epochIndex

        let gbToMainBlock :: Block SscGodTossing
            gbToMainBlock = Left . fst $ generatedBlocks

        let mainBlocks :: [MainBlock SscGodTossing]
            mainBlocks = snd generatedBlocks

        let mbToMainBlock :: [Block SscGodTossing]
            mbToMainBlock = Right <$> mainBlocks

        pure $ [gbToMainBlock] ++ mbToMainBlock

    generateEpochBlocks
        :: Maybe (BlockHeader SscGodTossing)
        -> SlotsPerEpoch
        -> EpochIndex
        -> (GenesisBlock SscGodTossing, [MainBlock SscGodTossing])
    generateEpochBlocks mBlockHeader slotsPerEpoch' epochIndex =
        (epochGenesisBlock, epochBlocks)
      where
        epochGenesisBlock :: GenesisBlock SscGodTossing
        epochGenesisBlock = mkGenesisBlock mBlockHeader epochIndex producedSlotLeaders

        epochBlocks :: [MainBlock SscGodTossing]
        epochBlocks =
            -- TODO(ks): Not correct, but I will fix it later.
            generateBlocks getPrevBlockHeader <$> blockNumbers
          where
            blockNumbers :: [Word]
            blockNumbers = [1..slotsPerEpoch']

            -- type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)
            getPrevBlockHeader :: BlockHeader SscGodTossing
            getPrevBlockHeader = getBlockHeader . Left $ epochGenesisBlock

            generateBlocks
                :: (HasConfiguration, HasUpdateConfiguration)
                => BlockHeader SscGodTossing
                -> BlockNumber
                -> MainBlock SscGodTossing
            generateBlocks previousBlockHeader blockNumber' =

                case basicBlock previousBlockHeader currentSecretKey slotId of
                    Left _      -> error "Block creation error!"
                    Right block ->
                        block & difficultyL .~ (ChainDifficulty $ BlockCount $ fromIntegral blockNumber')
              where

                slotId :: SlotId
                slotId = SlotId
                    { siEpoch = epochIndex
                    , siSlot  = UnsafeLocalSlotIndex integralBlockNumber
                    }

                currentSecretKey :: SecretKey
                currentSecretKey = secretKeys Prelude.!! integralBlockNumber

                integralBlockNumber :: Integral a => a
                integralBlockNumber = fromIntegral blockNumber'

-- type SlotLeaders   = NonEmpty StakeholderId
-- type StakeholderId = AbstractHash Blake2b_224 PublicKey

type SlotLeadersNumber = Word


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



