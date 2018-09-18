{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | A common interpretation for the UTxO DSL and abstract chain into Cardano
module UTxO.IntTrans
  (
    -- * Interpretation errors
    IntException(..)
    -- * Interpretation context
  , IntCheckpoint(..)
  , mkCheckpoint
  , createEpochBoundary
    -- * Interpretation monad transformer
  , ConIntT(..)
    -- * Interpretation type classes and a rollback
  , Interpretation(..)
  , Interpret(..)
  , IntRollback(..)
    -- * Translation context
  , constants
  , magic
  )
  where

import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock (..),
                     ResolvedTx (..))
import           Cardano.Wallet.Kernel.Types
                     (RawResolvedBlock (UnsafeRawResolvedBlock),
                     fromRawResolvedBlock)
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Reflection (give)
import qualified Formatting.Buildable
import           Pos.Chain.Block (BlockHeader)
import           Pos.Chain.Block
                     (BlockHeader (BlockHeaderGenesis, BlockHeaderMain),
                     GenesisBlock, gbHeader, genBlockLeaders, mkGenesisBlock)
import           Pos.Chain.Lrc (followTheSatoshi)
import           Pos.Chain.Txp (TxIn (..), TxOutAux (..), txOutStake)
import           Pos.Client.Txp.Util (TxError (..))
import           Pos.Core (ProtocolConstants, ProtocolMagic, SlotId (..))
import           Pos.Core.Common (Coin, SharedSeed (..), SlotLeaders,
                     StakeholderId, StakesList, StakesMap, addCoin, subCoin)
import           Pos.Core.Genesis (GenesisWStakeholders, gdBootStakeholders,
                     gdProtocolConsts,
                     genesisProtocolConstantsToProtocolConstants)
import           Pos.Core.Slotting (EpochIndex (..), crucialSlot)
import           Universum
import           UTxO.Context (CardanoContext (..), TransCtxt (..))
import           UTxO.Translate (TranslateT, withConfig)


{-------------------------------------------------------------------------------
  Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException =
    IntExNonOrdinaryAddress
  | IntExClassifyInputs Text
  | IntExMkDlg          Text
  | IntExCreateBlock    Text
  | IntExMkSlot         Text
  | IntExTx             TxError -- ^ Occurs during fee calculation
  | IntUnknownHash      Text
  | IntIndexOutOfRange  Text Word32 -- ^ During input resolution (hash and index)

    -- | Unknown stake holder during stake calculation
  | IntUnknownStakeholder StakeholderId

    -- | Coin overflow during stake calculation
    --
    -- We record the old stake and the stake we were suppose to add
  | IntStakeOverflow StakeholderId Coin Coin

    -- | Coin underflow during stake calculatino
    --
    -- We record the old stake and the stake we were supposed to subtract
  | IntStakeUnderflow StakeholderId Coin Coin

    -- | Attempt to rollback without previous checkpoints
  | IntCannotRollback
  deriving (Show)

instance Exception IntException

-- instance Buildable IntException where
--   build = bprint shown

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId        :: !SlotId

      -- | Header of the block in this slot
      --
      -- Will be initialized to the header of the genesis block.
    , icBlockHeader   :: !BlockHeader

      -- | Slot leaders for the current epoch
    , icEpochLeaders  :: !SlotLeaders

      -- | Running stakes
    , icStakes        :: !StakesMap

      -- | Snapshot of the stakes at the 'crucial' slot in the current epoch; in
      -- other words, the stakes used to compute the slot leaders for the next epoch.
    , icCrucialStakes :: !StakesMap
    }

{-------------------------------------------------------------------------------
  The interpretation monad
-------------------------------------------------------------------------------}

-- | Interpretation monad with a parameterised context
newtype ConIntT (ctxt :: (* -> *) -> *) h e m a = IntT {
    unIntT :: StateT (ctxt h) (TranslateT (Either IntException e) m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TransCtxt
           , MonadError (Either IntException e)
           )

-- | Evaluate state strictly
instance (ctxth ~ ctxt h, Monad m) => MonadState ctxth (ConIntT ctxt h e m) where
  get    = IntT $ get
  put !s = IntT $ put s

{-------------------------------------------------------------------------------
  Extract some values we need from the translation context
-------------------------------------------------------------------------------}

constants :: TransCtxt -> ProtocolConstants
constants = genesisProtocolConstantsToProtocolConstants
          . gdProtocolConsts
          . ccData
          . tcCardano

weights :: TransCtxt -> GenesisWStakeholders
weights = gdBootStakeholders . ccData . tcCardano

magic :: TransCtxt -> ProtocolMagic
magic = ccMagic . tcCardano


{-------------------------------------------------------------------------------
  Constructing checkpoints
-------------------------------------------------------------------------------}

mkCheckpoint :: Monad m
             => IntCheckpoint    -- ^ Previous checkpoint
             -> SlotId           -- ^ Slot of the new block just created
             -> RawResolvedBlock -- ^ The block just created
             -> TranslateT IntException m IntCheckpoint
mkCheckpoint prev slot raw@(UnsafeRawResolvedBlock block _inputs) = do
    pc <- asks constants
    gs <- asks weights
    let isCrucial = give pc $ slot == crucialSlot (siEpoch slot)
    newStakes <- updateStakes gs (fromRawResolvedBlock raw) (icStakes prev)
    return IntCheckpoint {
        icSlotId        = slot
      , icBlockHeader   = BlockHeaderMain $ block ^. gbHeader
      , icEpochLeaders  = icEpochLeaders prev
      , icStakes        = newStakes
      , icCrucialStakes = if isCrucial
                            then newStakes
                            else icCrucialStakes prev
      }

-- | Update the stakes map as a result of a block.
--
-- We follow the 'Stakes modification' section of the txp.md document.
updateStakes :: forall m. MonadError IntException m
             => GenesisWStakeholders
             -> ResolvedBlock
             -> StakesMap -> m StakesMap
updateStakes gs (ResolvedBlock txs _) =
    foldr (>=>) return $ map go txs
  where
    go :: ResolvedTx -> StakesMap -> m StakesMap
    go (ResolvedTx ins outs) =
        subStake >=> addStake
      where
        subStakes, addStakes :: [(StakeholderId, Coin)]
        subStakes = concatMap txOutStake' $ toList     (_fromDb ins)
        addStakes = concatMap txOutStake' $ Map.toList (_fromDb outs)

        subStake, addStake :: StakesMap -> m StakesMap
        subStake sm = foldM (opStake subCoin IntStakeUnderflow) sm subStakes
        addStake sm = foldM (opStake addCoin IntStakeOverflow)  sm addStakes

    opStake :: (Coin -> Coin -> Maybe Coin)
            -> (StakeholderId -> Coin -> Coin -> IntException)
            -> StakesMap
            -> (StakeholderId, Coin)
            -> m StakesMap
    opStake op exFun sm (hId, c) = do
        stake <- stakeOf hId sm
        case stake `op` c of
          Just stake' -> return $! HM.insert hId stake' sm
          Nothing     -> throwError $ exFun hId stake c

    stakeOf :: StakeholderId -> StakesMap -> m Coin
    stakeOf hId sm =
        case HM.lookup hId sm of
          Just s  -> return s
          Nothing -> throwError $ IntUnknownStakeholder hId

    txOutStake' :: (TxIn, TxOutAux) -> StakesList
    txOutStake' = txOutStake gs . toaOut . snd

-- | Create an epoch boundary block
--
-- In between each epoch there is an epoch boundary block (or EBB), that records
-- the stakes for the next epoch (in the Cardano codebase is referred to as a
-- "genesis block", and indeed the types are the same; we follow the terminology
-- from the spec here).
--
-- We /update/ the most recent checkpoint so that when we rollback, we effectively
-- rollback /two/ blocks. This is important, because the abstract chain has no
-- concept of EBBs.
createEpochBoundary :: Monad m
                    => IntCheckpoint
                    -> TranslateT IntException m (IntCheckpoint, GenesisBlock)
createEpochBoundary ic = do
    pm    <- asks magic
    pc    <- asks constants
    slots <- asks (ccEpochSlots . tcCardano)
    let newLeaders = give pc $ followTheSatoshi
                                 slots
                                 boringSharedSeed
                                 (HM.toList $ icCrucialStakes ic)
        ebb = mkGenesisBlock
                pm
                (Right     $ icBlockHeader ic)
                (nextEpoch $ icSlotId      ic)
                newLeaders
    return (
        ic { icEpochLeaders = ebb ^. genBlockLeaders
           , icBlockHeader  = BlockHeaderGenesis $ ebb ^. gbHeader
           }
      , ebb
      )
  where
    -- This is a shared seed which never changes. Obviously it is not an
    -- accurate reflection of how Cardano works.
    boringSharedSeed :: SharedSeed
    boringSharedSeed = SharedSeed "Static shared seed"

    nextEpoch :: SlotId -> EpochIndex
    nextEpoch (SlotId (EpochIndex i) _) = EpochIndex $ i + 1


-- | Gives a type for an interpretation context.
--
-- 'i' is a parameter binding interpretations for a given pair of a source
-- and a target language.
class Interpretation i where
  -- | Denotes an interpretation context, typically a monad transformer
  --
  -- - The first kind (* -> *) usually represents hash functions.
  -- - The second kind usually represents error types.
  -- - The third kind usually represents monad instances.
  -- - The fourth kind usually represents source types (the source of the
  --   translation).
  type IntCtx i :: (* -> *) -> * -> (* -> *) -> * -> *

-- | Interpretation of a source language to a target language, e.g. from
-- the UTxO DSL into Cardano.
--
-- 'fromTo' is a parameter binding interpretations for a given pair of a source
-- and a target language.
-- 'h' is a hash type.
-- 'a' is the source language's type being interpreted.
-- 'Interpreted fromTo a' is the target language's type being interpreted to.
class Interpretation fromTo => Interpret fromTo h a where
  type Interpreted fromTo a :: *

  -- | The single method of the type class that performs the interpretation
  int :: Monad m => a -> IntCtx fromTo h e m (Interpreted fromTo a)

-- | For convenience, we provide an event that corresponds to rollback
--
-- This makes interpreting DSL blocks and these "pseudo-DSL" rollbacks uniform.
data IntRollback = IntRollback
