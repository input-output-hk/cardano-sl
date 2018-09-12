{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Translation of an abstract chain into Cardano.
module Chain.Abstract.Translate.ToCardano
  ( IntT(..)
  , TranslateT(..)
  ) where

import           Cardano.Wallet.Kernel.Types
                 ( RawResolvedBlock(UnsafeRawResolvedBlock)
                 , RawResolvedTx(..)
                 , fromRawResolvedBlock
                 , mkRawResolvedBlock
                 , mkRawResolvedTx )
import           Cardano.Wallet.Kernel.DB.InDb (InDb(..))
import           Cardano.Wallet.Kernel.DB.Resolved
                 ( ResolvedBlock(..)
                 , ResolvedTx(..) )
import           Chain.Abstract
                 ( Block(..)
                 , Output(..)
                 , Transaction(..)
                 , hash
                 , outAddr )
import           Control.Lens ((%=), (.=), ix)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Data.Constraint (Dict(..))
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Reflection (give)
import           Pos.Chain.Block
                 ( BlockHeader(BlockHeaderMain, BlockHeaderGenesis)
                 , GenesisBlock
                 , MainBlock
                 , gbHeader
                 , genBlockLeaders
                 , mkGenesisBlock )
import           Pos.Chain.Lrc (followTheSatoshi)
import           Pos.Chain.Ssc (defaultSscPayload)
import           Pos.Chain.Txp
                 ( TxAux(..)
                 , TxId
                 , TxIn(..)
                 , TxOut(..)
                 , TxOutAux(..)
                 , txOutStake )
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Client.Txp.Util (makeMPubKeyTx, makeRedemptionTx)
import qualified Pos.Core as Core
                 ( HasConfiguration
                 , ProtocolConstants
                 , ProtocolMagic
                 , SlotId(..) )
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Common
                 ( Coin(..)
                 , SharedSeed(..)
                 , SlotLeaders
                 , StakeholderId
                 , StakesList
                 , StakesMap
                 , addCoin
                 , mkCoin
                 , subCoin )
import           Pos.Core.Delegation (DlgPayload(UnsafeDlgPayload))
import           Pos.Core.Genesis
                 ( GenesisWStakeholders
                 , gdBootStakeholders
                 , gdProtocolConsts
                 , genesisProtocolConstantsToProtocolConstants )
import           Pos.Core.Slotting
                 ( EpochIndex(..)
                 , addLocalSlotIndex
                 , crucialSlot
                 , localSlotIndexMaxBound
                 , localSlotIndexMinBound )
import           Pos.Crypto.Signing.Safe (SafeSigner(FakeSigner))
import qualified Pos.Crypto (hash)
import           Pos.DB.Block (RawPayload(..), createMainBlockPure)
import           Universum
import           UTxO.Context
                 ( Addr
                 , AddrInfo(..)
                 , BlockSignInfo(..)
                 , CardanoContext(..)
                 , TransCtxt(..)
                 , blockSignInfoForSlot
                 , resolveAddr )
import           UTxO.Crypto
                 ( ClassifiedInputs(InputsRegular, InputsRedeem)
                 , RedeemKeyPair(..)
                 , RegularKeyPair(..)
                 , SomeKeyPair
                 , TxOwnedInput
                 , classifyInputs )
import qualified UTxO.DSL as DSL
                 ( Hash
                 , Input(..)
                 , Transaction
                 , Value )
import           UTxO.Interpreter (Interpretation(..), Interpret(..))

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint = IntCheckpoint {
      -- | Slot number of this checkpoint
      icSlotId        :: !Core.SlotId

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

-- | Information we keep about the transactions we encounter
data TxMeta h = TxMeta {
  -- | The abstract transaction itself
  --
  -- We use this for input resolution.
  tmTx   :: !(Transaction h Addr)

  -- | Its Cardano hash
  --
  -- This is intentionally not a strict field because for the bootstrap
  -- transaction there is no corresponding TxId and this will be an error term.
, tmHash :: TxId
}

-- | Interpretation context
data IntCtxt h = IntCtxt {
  -- | Transaction map
  _icTx          :: !(Map (h (DSL.Transaction h Addr)) (TxMeta h))

  -- | Checkpoints
, _icCheckpoints :: !(NonEmpty IntCheckpoint)
}

makeLenses ''IntCtxt

{-------------------------------------------------------------------------------
  Extract some values we need from the translation context
-------------------------------------------------------------------------------}

constants :: TransCtxt -> Core.ProtocolConstants
constants = genesisProtocolConstantsToProtocolConstants
          . gdProtocolConsts
          . ccData
          . tcCardano

weights :: TransCtxt -> GenesisWStakeholders
weights = gdBootStakeholders . ccData . tcCardano

magic :: TransCtxt -> Core.ProtocolMagic
magic = ccMagic . tcCardano

{-------------------------------------------------------------------------------
Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException
  = IntExClassifyInputs   Text
  | IntExCreateBlock      Text
  | IntIndexOutOfRange    Text Word32 -- ^ During input resolution (hash and index)
    -- | Coin overflow during stake calculation
    --
    -- We record the old stake and the stake we were suppose to add
  | IntStakeOverflow      StakeholderId Coin Coin
    -- | Coin underflow during stake calculatino
    --
    -- We record the old stake and the stake we were supposed to subtract
  | IntStakeUnderflow     StakeholderId Coin Coin
  | IntUnknownHash        Text
    -- | Unknown stake holder during stake calculation
  | IntUnknownStakeholder StakeholderId
    deriving Show

instance Exception IntException

{-------------------------------------------------------------------------------
  Translation monad

  The translation provides access to the translation context as well as some
  dictionaries so that we can lift Cardano operations to the 'Translate' monad.
  (Eventually we may wish to do this differently.)
-------------------------------------------------------------------------------}

-- | Translation environment
data TranslateEnv = TranslateEnv {
      teContext :: TransCtxt
    , teConfig  :: Dict Core.HasConfiguration
    , teUpdate  :: Dict HasUpdateConfiguration
    }

newtype TranslateT e m a = TranslateT {
      unTranslateT :: ExceptT e (ReaderT TranslateEnv m) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           , MonadFail
           )

instance MonadTrans (TranslateT e) where
  lift = TranslateT . lift . lift

instance Monad m => MonadReader TransCtxt (TranslateT e m) where
  ask     = TranslateT $ asks teContext
  local f = TranslateT . local f' . unTranslateT
    where
      f' env = env { teContext = f (teContext env) }

{-------------------------------------------------------------------------------
  The interpretation monad
-------------------------------------------------------------------------------}

-- | Interpretation monad
newtype IntT h e m a = IntT {
    unIntT :: StateT (IntCtxt h) (TranslateT (Either IntException e) m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TransCtxt
           , MonadError (Either IntException e)
           )

-- | Evaluate state strictly
instance Monad m => MonadState (IntCtxt h) (IntT h e m) where
  get    = IntT $ get
  put !s = IntT $ put s

-- | Lift functions that want the configuration as type class constraints
withConfig :: Monad m
           => ((Core.HasConfiguration, HasUpdateConfiguration) => TranslateT e m a)
           -> TranslateT e m a
withConfig f = do
    Dict <- TranslateT $ asks teConfig
    Dict <- TranslateT $ asks teUpdate
    f

-- | Map errors
mapTranslateErrors :: Functor m
                   => (e -> e') -> TranslateT e m a -> TranslateT e' m a
mapTranslateErrors f (TranslateT ma) = TranslateT $ withExceptT f ma

-- | Convenience function to list actions in the 'Translate' monad
liftTranslateInt :: Monad m
                 => (   (Core.HasConfiguration, HasUpdateConfiguration)
                     => TranslateT IntException m a)
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left $ withConfig $ ta

{-------------------------------------------------------------------------------
  Convenience wrappers
-------------------------------------------------------------------------------}

-- | Increment slot ID
--
translateNextSlot :: Monad m => Core.SlotId -> TranslateT e m Core.SlotId
translateNextSlot (Core.SlotId epoch lsi) = withConfig $
    return $ case addLocalSlotIndex 1 lsi of
               Just lsi' -> Core.SlotId epoch       lsi'
               Nothing   -> Core.SlotId (epoch + 1) localSlotIndexMinBound

{-------------------------------------------------------------------------------
  Dealing with transactions
-------------------------------------------------------------------------------}

-- | Add a transaction into the context
putTx :: (DSL.Hash h Addr, Monad m)
      => Transaction h Addr
      -> TxId
      -> IntT h e m ()
putTx t txId = icTx %= Map.insert (hash t) TxMeta {
  tmTx   = t
, tmHash = txId
}

-- | Get a transaction from the context
getTx :: (DSL.Hash h Addr, Monad m)
      => h (DSL.Transaction h Addr)
      -> IntT h e m (TxMeta h)
getTx h = do
    txMap <- use icTx
    case Map.lookup h txMap of
      Nothing     -> throwError $ Left $ IntUnknownHash (pretty h)
      Just txMeta -> return txMeta

-- | Lookup the Cardano hash for the given DSL hash
intHash :: (DSL.Hash h Addr, Monad m)
        => h (DSL.Transaction h Addr) -> IntT h e m TxId
intHash = fmap tmHash . getTx

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr
                -> IntT h e m (Output h Addr)
inpSpentOutput' (DSL.Input h idx) =  do
  txMeta <- getTx h
  case toList (trOuts . tmTx $ txMeta) ^? ix (fromIntegral idx) of
    Nothing  -> throwError $ Left $ IntIndexOutOfRange (pretty h) idx
    Just out -> return out

{-------------------------------------------------------------------------------
  Dealing with checkpoints
-------------------------------------------------------------------------------}

-- | Push a new checkpoint
--
-- The checkpoint is created by the given function, which gets passed the previous
-- checkpoint as well as the slot ID that the next checkpoint should have.
--
-- The function runs in the underlying 'Translate' monad so that it is not tempted
-- to use state it shouldn't.
pushCheckpoint :: Monad m
                => (    (Core.HasConfiguration, HasUpdateConfiguration)
                     => IntCheckpoint
                     -> Core.SlotId
                     -> TranslateT IntException m (IntCheckpoint, a))
               -> IntT h e m a
pushCheckpoint f = do
    c :| cs  <- use icCheckpoints
    (c', a)  <- liftTranslateInt $ do
                  nextSlotId <- translateNextSlot (icSlotId c)
                  f c nextSlotId
    icCheckpoints .= c' :| c : cs
    return a

{-------------------------------------------------------------------------------
  Constructing checkpoints
-------------------------------------------------------------------------------}

-- | Creates a checkpoint for a regular block
mkCheckpoint :: Monad m
             => IntCheckpoint    -- ^ Previous checkpoint
             -> Core.SlotId      -- ^ Slot of the new block just created
             -> RawResolvedBlock -- ^ The block just created
             -> TranslateT IntException m IntCheckpoint
mkCheckpoint prev slot raw@(UnsafeRawResolvedBlock block _inputs) = do
    pc        <- asks constants
    gs        <- asks weights
    newStakes <- updateStakes gs (fromRawResolvedBlock raw) (icStakes prev)
    let isCrucial = give pc $ slot == crucialSlot (Core.siEpoch slot)
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

    nextEpoch :: Core.SlotId -> EpochIndex
    nextEpoch (Core.SlotId (EpochIndex i) _) = EpochIndex $ i + 1

{-------------------------------------------------------------------------------
  Translate the Abstract chain definitions to Cardano types
-------------------------------------------------------------------------------}

data Abstract2Cardano

instance Interpretation Abstract2Cardano where
  type IntCtx Abstract2Cardano = IntT

{-------------------------------------------------------------------------------
  Instances that read, but not update, the state
-------------------------------------------------------------------------------}

instance Interpret Abstract2Cardano h DSL.Value where
  type Interpreted Abstract2Cardano DSL.Value = Coin

  int :: Monad m => DSL.Value -> IntT h e m Coin
  int = return . mkCoin

instance Interpret Abstract2Cardano h Addr where
  type Interpreted Abstract2Cardano Addr = AddrInfo

  int :: Monad m => Addr -> IntT h e m AddrInfo
  int = asks . resolveAddr


instance Interpret Abstract2Cardano h (Output h Addr) where
  type Interpreted Abstract2Cardano (Output h Addr) = TxOutAux

  int :: Monad m
      => Output h Addr
      -> IntT h e m TxOutAux
  int Output{..} = do
    AddrInfo{..} <- (int @Abstract2Cardano) outAddr
    outVal'      <- (int @Abstract2Cardano) outVal
    return TxOutAux {
      toaOut = TxOut {
        txOutAddress = addrInfoCardano
      , txOutValue   = outVal'
      }
    }

instance DSL.Hash h Addr => Interpret Abstract2Cardano h (DSL.Input h Addr) where
  type Interpreted Abstract2Cardano (DSL.Input h Addr) =
    (TxOwnedInput SomeKeyPair, TxOutAux)

  int :: Monad m
      => DSL.Input h Addr -> IntT h e m (TxOwnedInput SomeKeyPair, TxOutAux)
  int inp@DSL.Input{..} = do
      -- We figure out who must sign the input by looking at the output
      spentOutput   <- inpSpentOutput' inp
      resolvedInput <- (int @Abstract2Cardano) spentOutput
      AddrInfo{..}  <- (int @Abstract2Cardano) $ outAddr spentOutput
      inpTrans'     <- intHash $ inpTrans
      return (
               (addrInfoAddrKey, TxInUtxo inpTrans' inpIndex)
             , resolvedInput
             )

{-------------------------------------------------------------------------------
  Instances that change the state

  NOTE: We need to be somewhat careful with using these instances. When
  interpreting a bunch of transactions, those blocks will be part of the
  context of whatever is interpreted next.
-------------------------------------------------------------------------------}

-- | Interpretation of transactions
instance DSL.Hash h Addr => Interpret Abstract2Cardano h (Transaction h Addr) where
  type Interpreted Abstract2Cardano (Transaction h Addr) = RawResolvedTx

  int :: forall e m. Monad m
      => Transaction h Addr -> IntT h e m RawResolvedTx
  int t = do
      (trIns', resolvedInputs) <- unzip . toList <$> mapM (int @Abstract2Cardano) (trIns  t)
      trOuts'                  <-         toList <$> mapM (int @Abstract2Cardano) (trOuts t)
      txAux   <- liftTranslateInt $ mkTx trIns' trOuts'
      putTx t $ Pos.Crypto.hash (taTx txAux)
      return $ mkRawResolvedTx txAux (NE.fromList resolvedInputs)
    where
      mkTx :: [TxOwnedInput SomeKeyPair]
           -> [TxOutAux]
           -> TranslateT IntException m TxAux
      mkTx inps outs = mapTranslateErrors IntExClassifyInputs $ do
        pm <- asks magic
        case classifyInputs inps of
          Left err ->
            throwError err
          Right (InputsRegular inps') -> withConfig $
            return . either absurd identity $
              makeMPubKeyTx
                pm
                (Right . FakeSigner . regKpSec)
                (NE.fromList inps')
                (NE.fromList outs)
          Right (InputsRedeem (kp, inp)) -> withConfig $
            return $
              makeRedemptionTx
                pm
                (redKpSec kp)
                (NE.fromList [inp])
                (NE.fromList outs)

-- | Interpretation of a block of transactions
instance DSL.Hash h Addr => Interpret Abstract2Cardano h (Block h Addr) where
  -- The block and the EBB, if any
  type Interpreted Abstract2Cardano (Block h Addr) = (RawResolvedBlock, Maybe GenesisBlock)

  int :: forall e m. Monad m
      => Block h Addr -> IntT h e m (RawResolvedBlock, Maybe GenesisBlock)
  int b = do
      let (OldestFirst txs) = blockTransactions b
      (txs', resolvedTxInputs) <- unpack <$> mapM (int @Abstract2Cardano) txs
      pushCheckpoint $ \prev slot -> do
        pc    <- asks constants
        block <- mkBlock
                   (icEpochLeaders prev)
                   (icBlockHeader  prev)
                   slot
                   txs'
        let raw = mkRawResolvedBlock block resolvedTxInputs
        checkpoint <- mkCheckpoint prev slot raw
        if isEpochBoundary pc slot
          then second (\ebb -> (raw, Just ebb)) <$> createEpochBoundary checkpoint
          else return (checkpoint, (raw, Nothing))
    where
      unpack :: [RawResolvedTx] -> ([TxAux], [NonEmpty TxOutAux])
      unpack = unzip . map (rawResolvedTx &&& rawResolvedTxInputs)

      isEpochBoundary :: Core.ProtocolConstants -> Core.SlotId -> Bool
      isEpochBoundary pc slot = Core.siSlot slot == localSlotIndexMaxBound pc

      mkBlock :: (Core.HasConfiguration, HasUpdateConfiguration)
              => SlotLeaders
              -> BlockHeader
              -> Core.SlotId
              -> [TxAux]
              -> TranslateT IntException m MainBlock
      mkBlock leaders prev slotId ts = mapTranslateErrors IntExCreateBlock $ do
        -- TODO: empty delegation payload
        let dlgPayload = UnsafeDlgPayload []

        -- empty update payload
        let updPayload = def

        -- figure out who needs to sign the block
        BlockSignInfo{..} <- asks $ blockSignInfoForSlot leaders slotId

        pm <- asks magic
        createMainBlockPure
          pm
          blockSizeLimit
          prev
          (Just (bsiPSK, bsiLeader))
          slotId
          bsiKey
          (RawPayload
              (toList ts)
              (defaultSscPayload (Core.siSlot slotId)) -- TODO
              dlgPayload
              updPayload
            )

      -- TODO: Get this value from somewhere rather than hardcoding it
      blockSizeLimit = 2 * 1024 * 1024 -- 2 MB
