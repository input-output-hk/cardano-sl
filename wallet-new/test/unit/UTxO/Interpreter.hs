{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Interpreter from the DSL to Cardano types
module UTxO.Interpreter (
    -- * Interpretation errors
    IntException(..)
    -- * Interpretation context
  , IntCtxt -- opaque
  , initIntCtxt
    -- * Interpretation monad
  , IntT
  , runIntT
  , runIntT'
  , runIntBoot
  , runIntBoot'
    -- * Interpreter proper
  , Interpret(..)
  , IntRollback(..)
  ) where

import           Universum hiding (id)

import           Control.Arrow ((&&&))
import           Control.Lens ((%=), (.=))
import           Control.Lens.TH (makeLenses)
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Reflection (give)
import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable
import           Prelude (Show (..))
import           Serokell.Util (listJson, mapJson)

import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved
import           Cardano.Wallet.Kernel.Types
import           Cardano.Wallet.Kernel.Util (at)

import           Pos.Chain.Lrc (followTheSatoshi)
import           Pos.Chain.Ssc (defaultSscPayload)
import           Pos.Chain.Txp (Utxo, txOutStake)
import           Pos.Chain.Update
import           Pos.Client.Txp
import           Pos.Core
import           Pos.Core.Block (Block, BlockHeader (..), GenesisBlock,
                     MainBlock, gbHeader, genBlockLeaders, mkGenesisBlock)
import           Pos.Core.Chrono
import           Pos.Core.Delegation (DlgPayload (..))
import           Pos.Core.Genesis (GenesisWStakeholders, gdBootStakeholders,
                     gdProtocolConsts,
                     genesisProtocolConstantsToProtocolConstants)
import           Pos.Core.Txp (TxAux (..), TxId, TxIn (..), TxOut (..),
                     TxOutAux (..))
import           Pos.Crypto
import           Pos.DB.Block (RawPayload (..), createMainBlockPure)

import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.Crypto
import qualified UTxO.DSL as DSL
import           UTxO.Translate

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

instance Buildable IntException where
  build = bprint shown

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Information we keep about the transactions we encounter
data TxMeta h = TxMeta {
      -- | The DSL transaction itself
      --
      -- We use this for input resolution.
      tmTx   :: !(DSL.Transaction h Addr)

      -- | Its Cardano hash
      --
      -- This is intentionally not a strict field because for the bootstrap
      -- transaction there is no corresponding TxId and this will be an error term.
    , tmHash :: TxId
    }

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

-- | Interpretation context
data IntCtxt h = IntCtxt {
      -- | Transaction metadata
      _icTxMeta      :: !(Map (h (DSL.Transaction h Addr)) (TxMeta h))

      -- | Checkpoints
    , _icCheckpoints :: !(NonEmpty IntCheckpoint)
    }

makeLenses ''IntCtxt

-- | Initial interpretation context
--
-- NOTE: In Cardano there is no equivalent of the boot transaction and hence
-- no hash of the boot transaction.
initIntCtxt :: (Monad m, DSL.Hash h Addr)
            => DSL.Transaction h Addr -> TranslateT e m (IntCtxt h)
initIntCtxt boot = do
    genesis     <- BlockHeaderGenesis <$> translateGenesisHeader
    leaders     <- asks (ccInitLeaders . tcCardano)
    initStakes  <- asks (ccStakes      . tcCardano)
    return $ IntCtxt {
          _icTxMeta = Map.singleton (DSL.hash boot) TxMeta {
               tmTx   = boot
             , tmHash = error "initIntCtxt: bootstrap transaction has no Cardano hash"
             }
        , _icCheckpoints = IntCheckpoint {
              icSlotId        = translateFirstSlot
            , icBlockHeader   = genesis
            , icEpochLeaders  = leaders
            , icStakes        = initStakes
            , icCrucialStakes = initStakes
            } :| []
        }

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
  The interpretation monad

  NOTE: It is important to limit the scope of 'IntM'. See comments below
  about the instances of 'Interpret' that update the state.
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

-- | Run the interpreter monad
runIntT :: IntCtxt h
        -> IntT h e m a
        -> TranslateT (Either IntException e) m (a, IntCtxt h)
runIntT ic ma = runStateT (unIntT ma) ic

-- | Variation on 'runIntT' when we can /only/ have interpretation exceptions
runIntT' :: Functor m
         => IntCtxt h
         -> IntT h Void m a
         -> TranslateT IntException m (a, IntCtxt h)
runIntT' ic = mapTranslateErrors mustBeLeft . runIntT ic

-- | Run the interpreter monad, given only the boot transaction
runIntBoot :: (Monad m, DSL.Hash h Addr)
           => DSL.Transaction h Addr
           -> IntT h e m a
           -> TranslateT (Either IntException e) m (a, IntCtxt h)
runIntBoot boot ma = do
    ic <- mapTranslateErrors Left $ initIntCtxt boot
    runIntT ic ma

-- | Variation on 'runIntBoot' when we can /only/ have interpretation exceptions
runIntBoot' :: (Monad m, DSL.Hash h Addr)
            => DSL.Transaction h Addr
            -> IntT h Void m a
            -> TranslateT IntException m (a, IntCtxt h)
runIntBoot' boot = mapTranslateErrors mustBeLeft . runIntBoot boot

-- | Convenience function to list actions in the 'Translate' monad
liftTranslateInt :: Monad m
                 => (   (HasConfiguration, HasUpdateConfiguration)
                     => TranslateT IntException m a)
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left $ withConfig $ ta

{-------------------------------------------------------------------------------
  Dealing with the transaction metadata
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTxMeta :: forall h e m. (DSL.Hash h Addr, Monad m)
          => DSL.Transaction h Addr -> TxId -> IntT h e m ()
putTxMeta t id = icTxMeta %= Map.insert (DSL.hash t) TxMeta {
       tmTx   = t
     , tmHash = id
     }

getTxMeta :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr) -> IntT h e m (TxMeta h)
getTxMeta h = do
    txMeta <- use icTxMeta
    case Map.lookup h txMeta of
      Nothing -> throwError $ Left $ IntUnknownHash (pretty h)
      Just m  -> return m

-- | Lookup a transaction by hash
findHash' :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr) -> IntT h e m (DSL.Transaction h Addr)
findHash' = fmap tmTx . getTxMeta

-- | Lookup the Cardano hash for the given DSL hash
intHash :: (Monad m, DSL.Hash h Addr)
        => h (DSL.Transaction h Addr) -> IntT h e m TxId
intHash = fmap tmHash . getTxMeta

-- | Resolve an input
inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr -> IntT h e m (DSL.Output h Addr)
inpSpentOutput' (DSL.Input h ix) =  do
    tx <- findHash' h
    case DSL.trOuts tx `at` fromIntegral ix of
      Nothing  -> throwError $ Left $ IntIndexOutOfRange (pretty h) ix
      Just out -> return out

{-------------------------------------------------------------------------------
  Dealing with checkpoints
-------------------------------------------------------------------------------}

-- | Pop off a checkpoint (in response to a rollback event)
popIntCheckpoint :: Monad m => IntT h e m ()
popIntCheckpoint = do
    st <- get
    case st ^. icCheckpoints of
      _c :| []     -> throwError $ Left IntCannotRollback
      _c :| c : cs -> put $ st & icCheckpoints .~ c :| cs

-- | Push a new checkpoint
--
-- The checkpoint is created by the given function, which gets passed the previous
-- checkpoint as well as the slot ID that the next checkpoint should have.
--
-- The function runs in the underlying 'Translate' monad so that it is not tempted
-- to use state it shouldn't.
pushCheckpoint :: Monad m
                => (    (HasConfiguration, HasUpdateConfiguration)
                     => IntCheckpoint
                     -> SlotId
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
        subStake sm = foldM (flip subStake1) sm subStakes
        addStake sm = foldM (flip addStake1) sm addStakes

    subStake1, addStake1 :: (StakeholderId, Coin) -> StakesMap -> m StakesMap
    subStake1 (id, c) sm = do
        stake <- stakeOf id sm
        case stake `subCoin` c of
          Just stake' -> return $! HM.insert id stake' sm
          Nothing     -> throwError $ IntStakeUnderflow id stake c
    addStake1 (id, c) sm = do
        stake <- stakeOf id sm
        case stake `addCoin` c of
          Just stake' -> return $! HM.insert id stake' sm
          Nothing     -> throwError $ IntStakeOverflow id stake c

    stakeOf :: StakeholderId -> StakesMap -> m Coin
    stakeOf id sm =
        case HM.lookup id sm of
          Just s  -> return s
          Nothing -> throwError $ IntUnknownStakeholder id

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
-- rollback /two/ blocks. This is important, because the DSL has no concept of these
-- EBBs.
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

{-------------------------------------------------------------------------------
  Translate the DSL UTxO definitions to Cardano types

  NOTE: Delegation in Cardano is described in the document
  "Delegation and Stake Locking in Cardano SL"
  <cardano-sl-articles/delegation/pdf/article.pdf>.
-------------------------------------------------------------------------------}

-- | Interpretation of the UTxO DSL
class Interpret h a where
  type Interpreted a :: *

  int :: Monad m => a -> IntT h e m (Interpreted a)

{-------------------------------------------------------------------------------
  Instances that read, but not update, the state
-------------------------------------------------------------------------------}

instance Interpret h DSL.Value where
  type Interpreted DSL.Value = Coin

  int :: Monad m => DSL.Value -> IntT h e m Coin
  int = return . mkCoin

instance Interpret h Addr where
  type Interpreted Addr = AddrInfo

  int :: Monad m => Addr -> IntT h e m AddrInfo
  int = asks . resolveAddr

instance DSL.Hash h Addr => Interpret h (DSL.Input h Addr) where
  type Interpreted (DSL.Input h Addr) = (TxOwnedInput SomeKeyPair, ResolvedInput)

  int :: Monad m
      => DSL.Input h Addr -> IntT h e m (TxOwnedInput SomeKeyPair, ResolvedInput)
  int inp@DSL.Input{..} = do
      -- We figure out who must sign the input by looking at the output
      spentOutput   <- inpSpentOutput' inp
      resolvedInput <- int spentOutput
      isBootstrap   <- isBootstrapTransaction <$> findHash' inpTrans

      if isBootstrap
        then do
          AddrInfo{..} <- int $ DSL.outAddr spentOutput
          -- See explanation at 'bootstrapTransaction'
          return (
                   (addrInfoAddrKey, TxInUtxo (unsafeHash addrInfoCardano) 0)
                 , resolvedInput
                 )
        else do
          AddrInfo{..} <- int     $ DSL.outAddr spentOutput
          inpTrans'    <- intHash $ inpTrans
          return (
                   (addrInfoAddrKey, TxInUtxo inpTrans' inpIndex)
                 , resolvedInput
                 )

instance Interpret h (DSL.Output h Addr) where
  type Interpreted (DSL.Output h Addr) = TxOutAux

  int :: Monad m
      => DSL.Output h Addr -> IntT h e m TxOutAux
  int DSL.Output{..} = do
      AddrInfo{..} <- int outAddr
      outVal'      <- int outVal
      return TxOutAux {
          toaOut = TxOut {
              txOutAddress = addrInfoCardano
            , txOutValue   = outVal'
            }
        }

instance DSL.Hash h Addr => Interpret h (DSL.Utxo h Addr) where
  type Interpreted (DSL.Utxo h Addr) = Utxo

  int :: forall e m. Monad m
      => DSL.Utxo h Addr -> IntT h e m Utxo
  int = fmap Map.fromList . mapM aux . DSL.utxoToList
    where
      aux :: (DSL.Input h Addr, DSL.Output h Addr)
          -> IntT h e m (TxIn, TxOutAux)
      aux (inp, out) = do
          ((_key, inp'), _) <- int inp
          out'              <- int out
          return (inp', out')

{-------------------------------------------------------------------------------
  Instances that change the state

  NOTE: We need to be somewhat careful with using these instances. When
  interpreting a bunch of transactions, those blocks will be part of the
  context of whatever is interpreted next.
-------------------------------------------------------------------------------}

-- | Interpretation of transactions
instance DSL.Hash h Addr => Interpret h (DSL.Transaction h Addr) where
  type Interpreted (DSL.Transaction h Addr) = RawResolvedTx

  int :: forall e m. Monad m
      => DSL.Transaction h Addr -> IntT h e m RawResolvedTx
  int t = do
      (trIns', resolvedInputs) <- unzip <$> mapM int (DSL.trIns' t)
      trOuts'                  <-           mapM int (DSL.trOuts t)
      txAux   <- liftTranslateInt $ mkTx trIns' trOuts'
      putTxMeta t $ hash (taTx txAux)
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

-- | Interpretation of a block of transactions, oldest first
instance DSL.Hash h Addr => Interpret h (DSL.Block h Addr) where
  -- The block and the EBB, if any
  type Interpreted (DSL.Block h Addr) = (RawResolvedBlock, Maybe GenesisBlock)

  int :: forall e m. Monad m
      => DSL.Block h Addr -> IntT h e m (RawResolvedBlock, Maybe GenesisBlock)
  int (OldestFirst txs) = do
      (txs', resolvedTxInputs) <- unpack <$> mapM int txs
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
      unpack :: [RawResolvedTx] -> ([TxAux], [ResolvedTxInputs])
      unpack = unzip . map (rawResolvedTx &&& rawResolvedTxInputs)

      isEpochBoundary :: ProtocolConstants -> SlotId -> Bool
      isEpochBoundary pc slot = siSlot slot == localSlotIndexMaxBound pc

      mkBlock :: (HasConfiguration, HasUpdateConfiguration)
              => SlotLeaders
              -> BlockHeader
              -> SlotId
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
              (defaultSscPayload (siSlot slotId)) -- TODO
              dlgPayload
              updPayload
            )

      -- TODO: Get this value from somewhere rather than hardcoding it
      blockSizeLimit = 2 * 1024 * 1024 -- 2 MB

instance DSL.Hash h Addr => Interpret h (DSL.Chain h Addr) where
  type Interpreted (DSL.Chain h Addr) = OldestFirst [] Block

  int :: forall e m. Monad m
      => DSL.Chain h Addr -> IntT h e m (OldestFirst [] Block)
  int (OldestFirst blocks) =
      OldestFirst . concatMap flatten <$> mapM int blocks
    where
      flatten :: (RawResolvedBlock, Maybe GenesisBlock) -> [Block]
      flatten (b, Nothing)  = [Right (rawResolvedBlock b)]
      flatten (b, Just ebb) = [Right (rawResolvedBlock b), Left ebb]

-- | For convenience, we provide an event that corresponds to rollback
--
-- This makes interpreting DSL blocks and these "pseudo-DSL" rollbacks uniform.
data IntRollback = IntRollback

instance Interpret h IntRollback where
  type Interpreted IntRollback = ()

  int :: Monad m => IntRollback -> IntT h e m ()
  int IntRollback = popIntCheckpoint

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeLeft :: Either a Void -> a
mustBeLeft (Left  a) = a
mustBeLeft (Right b) = absurd b

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

-- | NOTE: This is only for debugging. We print only a small subset.
instance Buildable (IntCtxt h) where
  build IntCtxt{..} = bprint
    ( "IntCtxt {"
    % "  checkpoints: " % listJson
    % "}"
    )
    _icCheckpoints

instance Buildable IntCheckpoint where
  build IntCheckpoint{..} = bprint
    ( "Checkpoint {"
    % "  slotId: " % build
    % ", stakes: " % mapJson
    )
    icSlotId
    icStakes
