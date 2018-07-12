{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}

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
  , liftTranslate
  , liftTranslateInt
    -- * Interpreter proper
  , Interpret(..)
  ) where

import           Universum hiding (id)

import           Control.Arrow ((&&&))
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Reflection (give)
import qualified Data.Set as Set
import           Formatting (bprint, shown)
import qualified Formatting.Buildable
import           Prelude (Show (..))

import           Cardano.Wallet.Kernel.DB.Resolved
import           Cardano.Wallet.Kernel.Types

import           Pos.Block.Logic
import           Pos.Client.Txp
import           Pos.Core
import           Pos.Core.Block (mkGenesisBlock)
import           Pos.Core.Chrono
import           Pos.Crypto
import           Pos.Lrc.Fts (followTheSatoshi)
import           Pos.Ssc (defaultSscPayload)
import           Pos.Txp.Base (txOutStake)
import           Pos.Txp.Toil
import           Pos.Update

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
  deriving (Show)

instance Exception IntException

instance Buildable IntException where
  build = bprint shown

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Interpretation context
data IntCtxt h = IntCtxt {
      -- | Ledger we have interpreted so far
      --
      -- This is needed to resolve DSL hashes to DSL transactions.
      icLedger        :: !(DSL.Ledger h Addr)

      -- | Mapping from DSL hashes to Cardano hashes
    , icHashes        :: !(Map (h (DSL.Transaction h Addr)) TxId)

      -- | Current epoch
    , icEpoch         :: !EpochIndex

      -- | Slot number for the next block to be translated
    , icNextSlot      :: !SlotId

      -- | The header of the last block we translated
      --
      -- In other words, the " previous " pointer for the next block to be
      -- translated.
      --
      -- Will be initialized to the header of the genesis block.
    , icPrevBlock     :: !BlockHeader

      -- | Slot leaders for the current epoch
    , icEpochLeaders  :: !SlotLeaders

      -- | Running stakes
    , icStakes        :: !StakesMap

      -- | Snapshot of the stakes at the 'crucial' slot in the current epoch; in
      -- other words, the stakes used to compute the slot leaders for the next epoch.
    , icCrucialStakes :: !StakesMap
    }

-- | Initial interpretation context
--
-- NOTE: In Cardano there is no equivalent of the boot transaction and hence
-- no hash of the boot transaction.
initIntCtxt :: Monad m => DSL.Transaction h Addr -> TranslateT IntException m (IntCtxt h)
initIntCtxt boot = do
    firstSlot <- mapTranslateErrors IntExMkSlot $ translateFirstSlot
    genesis   <- BlockHeaderGenesis <$> translateGenesisHeader
    leaders   <- asks (ccInitLeaders . tcCardano)
    initStakes <- asks (ccStakes . tcCardano)
    return $ IntCtxt {
          icLedger       = DSL.ledgerSingleton boot
        , icHashes       = Map.empty
        , icNextSlot     = firstSlot
        , icPrevBlock    = genesis
        , icEpochLeaders = leaders
        , icStakes       = initStakes
        , icCrucialStakes = initStakes
        , icEpoch        = EpochIndex 0
        }

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
runIntBoot :: Monad m
           => DSL.Transaction h Addr
           -> IntT h e m a
           -> TranslateT (Either IntException e) m (a, IntCtxt h)
runIntBoot boot ma = do
    ic <- mapTranslateErrors Left $ initIntCtxt boot
    runIntT ic ma

-- | Variation on 'runIntBoot' when we can /only/ have interpretation exceptions
runIntBoot' :: Monad m
            => DSL.Transaction h Addr
            -> IntT h Void m a
            -> TranslateT IntException m (a, IntCtxt h)
runIntBoot' boot = mapTranslateErrors mustBeLeft . runIntBoot boot

{-------------------------------------------------------------------------------
  Internal monad functions
-------------------------------------------------------------------------------}

liftTranslate :: Monad m
              => (   (HasConfiguration, HasUpdateConfiguration)
                  => TranslateT e m a)
              -> IntT h e m a
liftTranslate ta = IntT $ lift $ mapTranslateErrors Right $ withConfig $ ta

-- | Variation on @liftTranslate@ for translation functions that actually
-- may throw 'IntException's
liftTranslateInt :: Monad m
                 => (   (HasConfiguration, HasUpdateConfiguration)
                     => TranslateT IntException m a)
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left $ withConfig $ ta

-- | Add transaction into the context
pushTx :: forall h e m. (DSL.Hash h Addr, Monad m)
       => (DSL.Transaction h Addr, TxId) -> IntT h e m ()
pushTx (t, id) = do
    gs <- asks (gdBootStakeholders . ccData . tcCardano)
    ledger <- gets icLedger
    inputSpentOutputs <- mapM int $ catMaybes $ flip DSL.inpSpentOutput ledger <$> Set.toList (DSL.trIns t)
    outputs <- mapM int $ DSL.trOuts t
    modify $ aux (txModifyStakes gs inputSpentOutputs outputs)
  where
    aux :: (StakesMap -> StakesMap) -> IntCtxt h -> IntCtxt h
    aux smu ic =
        IntCtxt
        { icLedger       = DSL.ledgerAdd t            (icLedger ic)
        , icHashes       = Map.insert (DSL.hash t) id (icHashes ic)
        , icNextSlot     = icNextSlot     ic
        , icPrevBlock    = icPrevBlock    ic
        , icEpochLeaders = icEpochLeaders ic
        , icStakes       = newStakes
        , icCrucialStakes = newStakes
        , icEpoch = icEpoch ic
        }
      where
        newStakes = smu $ icStakes ic

    -- Update the stakes map as a result of this transaction.
    --
    -- We follow the 'Stakes modification' section of the txp.md document.
    txModifyStakes :: GenesisWStakeholders -> [TxOutAux] -> [TxOutAux] -> StakesMap -> StakesMap
    txModifyStakes gs inputSpentOutputs outputs = let
        inputStakes  = (txOutStake gs . toaOut) =<< inputSpentOutputs
        outputStakes = (txOutStake gs . toaOut) =<< outputs
        plusStake  sm' = foldl' (flip . uncurry $ HM.insertWith (flip unsafeAddCoin)) sm' outputStakes
        minusStake sm' = foldl' (flip . uncurry $ HM.insertWith (flip unsafeSubCoin)) sm' inputStakes
      in (plusStake . minusStake)

-- | Add an epoch boundary into the context.
--
--   This sets the "previous block" header, increased the next slot number, and
--   increases the epoch.
pushEpochBoundary :: forall h e m. Monad m => GenesisBlock -> IntT h e m ()
pushEpochBoundary ebb = put =<< liftTranslateInt . aux =<< get
  where
    aux :: IntCtxt h -> TranslateT IntException m (IntCtxt h)
    aux ic = do
      nextSlot' <- mapTranslateErrors IntExMkSlot $ translateNextSlot (icNextSlot ic)
      return $ ic
        { icNextSlot = nextSlot'
        , icEpochLeaders = ebb ^. genBlockLeaders
        , icPrevBlock = BlockHeaderGenesis $ ebb ^. gbHeader
        , icEpoch = ebb ^. genBlockEpoch
        }

-- | Add a block into the context
--
-- This sets the " previous block " header and increases the next slot number.
pushBlock :: forall h e m. Monad m => MainBlock -> IntT h e m ()
pushBlock block = put =<< (liftTranslateInt . aux) =<< get
  where
    aux :: IntCtxt h -> TranslateT IntException m (IntCtxt h)
    aux ic = mapTranslateErrors IntExMkSlot $ do
        pc <- asks (genesisProtocolConstantsToProtocolConstants . gdProtocolConsts . ccData . tcCardano)
        let crucialStakes = if isCrucialSlot ic pc
                then icStakes ic
                else icCrucialStakes ic
        nextSlot' <- translateNextSlot (icNextSlot ic)
        return IntCtxt {
            icLedger    = icLedger ic
          , icHashes    = icHashes ic
          , icNextSlot  = nextSlot'
          , icPrevBlock = BlockHeaderMain $ block ^. gbHeader
          , icEpochLeaders = icEpochLeaders ic
          , icStakes = icStakes ic
          , icCrucialStakes = crucialStakes
          , icEpoch = icEpoch ic
          }
    isCrucialSlot :: IntCtxt h -> ProtocolConstants -> Bool
    isCrucialSlot s pc = give pc $ icNextSlot s == crucialSlot (icEpoch s)

intHash :: (Monad m, DSL.Hash h Addr)
        => h (DSL.Transaction h Addr) -> IntT h e m TxId
intHash h = do
    mId <- Map.lookup h . icHashes <$> get
    case mId of
      Just id -> return id
      Nothing -> throwError . Left $ IntUnknownHash (pretty h)

{-------------------------------------------------------------------------------
  Lift some DSL operations that require a ledger to operations that
  take the IntCtxt
-------------------------------------------------------------------------------}

findHash' :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr) -> IntT h e m (DSL.Transaction h Addr)
findHash' h = (DSL.findHash' h . icLedger) <$> get

inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr -> IntT h e m (DSL.Output h Addr)
inpSpentOutput' inp = (DSL.inpSpentOutput' inp . icLedger) <$> get

{-------------------------------------------------------------------------------
  Translate the DSL UTxO definitions to Cardano types

  NOTE: Delegation in Cardano is described in the document
  "Delegation and Stake Locking in Cardano SL"
  <cardano-sl-articles/delegation/pdf/article.pdf>.
-------------------------------------------------------------------------------}

-- | Interpretation of the UTxO DSL
class Interpret h a where
  type Interpreted a :: *

  int :: (Monad m)
      => a -> IntT h e m (Interpreted a)

{-------------------------------------------------------------------------------
  Instances that read, but not update, the state
-------------------------------------------------------------------------------}

instance Interpret h DSL.Value where
  type Interpreted DSL.Value = Coin

  int :: (Monad m) => DSL.Value -> IntT h e m Coin
  int = return . mkCoin

instance Interpret h Addr where
  type Interpreted Addr = AddrInfo

  int :: (Monad m) => Addr -> IntT h e m AddrInfo
  int = asks . resolveAddr

instance DSL.Hash h Addr => Interpret h (DSL.Input h Addr) where
  type Interpreted (DSL.Input h Addr) = (TxOwnedInput SomeKeyPair, ResolvedInput)

  int :: (Monad m)
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

  int :: forall e m. (Monad m)
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

  int :: forall e m. (Monad m)
      => DSL.Transaction h Addr -> IntT h e m RawResolvedTx
  int t = do
      (trIns', resolvedInputs) <- unzip <$> mapM int (DSL.trIns' t)
      trOuts'                  <-           mapM int (DSL.trOuts t)
      txAux   <- liftTranslateInt $ mkTx trIns' trOuts'
      pushTx (t, hash (taTx txAux))
      return $ mkRawResolvedTx txAux (NE.fromList resolvedInputs)
    where
      mkTx :: [TxOwnedInput SomeKeyPair]
           -> [TxOutAux]
           -> TranslateT IntException m TxAux
      mkTx inps outs = mapTranslateErrors IntExClassifyInputs $
        case classifyInputs inps of
          Left err ->
            throwError err
          Right (InputsRegular inps') -> withProtocolMagic $ \pm -> withConfig $
            return . either absurd identity $
              makeMPubKeyTx
                pm
                (Right . FakeSigner . regKpSec)
                (NE.fromList inps')
                (NE.fromList outs)
          Right (InputsRedeem (kp, inp)) -> withProtocolMagic $ \pm -> withConfig $
            return $
              makeRedemptionTx
                pm
                (redKpSec kp)
                (NE.fromList [inp])
                (NE.fromList outs)

-- | Interpretation of a list of transactions, oldest first
--
-- Each transaction becomes part of the context for the next.
instance DSL.Hash h Addr => Interpret h (DSL.Block h Addr) where
  type Interpreted (DSL.Block h Addr) = RawResolvedBlock

  int :: forall e m. (Monad m)
      => DSL.Block h Addr -> IntT h e m RawResolvedBlock
  int (OldestFirst txs) = do
      -- Create an epoch boundary block on the epoch boundary
      ic <- get
      mebb <- if isEpochBoundary ic
              then do
                (pc, slots) <- liftTranslateInt $ do
                  cc <- asks tcCardano
                  return ( genesisProtocolConstantsToProtocolConstants . gdProtocolConsts $ ccData cc
                         , ccEpochSlots cc
                         )
                let newLeaders = give pc $ followTheSatoshi slots boringSharedSeed (HM.toList $ icCrucialStakes ic)
                    newEpoch = nextEpoch $ icEpoch ic
                    sbb = mkGenesisBlock testProtocolMagic (Right $ icPrevBlock ic) newEpoch newLeaders
                pushEpochBoundary sbb >> return (Just sbb)
              else return Nothing

      (txs', resolvedTxInputs) <- unpack <$> mapM int txs
      leaders <- gets icEpochLeaders
      prev    <- gets icPrevBlock
      slot    <- gets icNextSlot
      block   <- liftTranslateInt $ mkBlock leaders prev slot txs'
      pushBlock block
      return $ mkRawResolvedBlock block mebb resolvedTxInputs
    where
      unpack :: [RawResolvedTx] -> ([TxAux], [ResolvedTxInputs])
      unpack = unzip . map (rawResolvedTx &&& rawResolvedTxInputs)

      isEpochBoundary s = icEpoch s /= siEpoch (icNextSlot s)

      -- | This is a shared seed which never changes. Obviously it is not an
      -- accurate reflection of how Cardano works.
      boringSharedSeed :: SharedSeed
      boringSharedSeed = SharedSeed "Static shared seed"

      -- | Protocol magic for use in tests.
      testProtocolMagic :: ProtocolMagic
      testProtocolMagic = ProtocolMagic 21345

      nextEpoch :: EpochIndex -> EpochIndex
      nextEpoch (EpochIndex i) = EpochIndex $ i + 1

      mkBlock :: SlotLeaders
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

        withProtocolMagic $ \pm ->
          withConfig $
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

  int :: forall e m. (Monad m)
      => DSL.Chain h Addr -> IntT h e m (OldestFirst [] Block)
  int (OldestFirst blocks) = OldestFirst . join <$>
      mapM (fmap blockWithBoundary . int) blocks
    where
      blockWithBoundary (UnsafeRawResolvedBlock block Nothing _) = [Right block]
      blockWithBoundary (UnsafeRawResolvedBlock block (Just ebb) _ ) = [Left ebb, Right block]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeLeft :: Either a Void -> a
mustBeLeft (Left  a) = a
mustBeLeft (Right b) = absurd b
