{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}

-- | Interpreter from the DSL to Cardano types
module UTxO.Interpreter (
    -- * Interpretation errors
    IntException(..)
    -- * Interpretation context
  , IntCtxt -- opaque
    -- * Interpretation monad
  , IntT
  , runIntT
  , runIntBoot
    -- * Interpreter proper
  , Interpret(..)
  ) where

import           Data.Default (def)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text.Buildable
import           Formatting (bprint, shown)
import           Prelude (Show (..))
import           Universum

import           Cardano.Wallet.Kernel.Types

import           Pos.Block.Logic
import           Pos.Client.Txp
import           Pos.Core
import           Pos.Crypto
import           Pos.Ssc (defaultSscPayload)
import           Pos.Txp.Toil
import           Pos.Update
import           Pos.Util.Chrono

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
      icLedger :: DSL.Ledger h Addr

      -- | Mapping from DSL hashes to Cardano hashes
    , icHashes :: Map (h (DSL.Transaction h Addr)) TxId

      -- | Slot number for the next block to be translated
    , icNextSlot :: SlotId

      -- | The header of the last block we translated
      --
      -- In other words, the " previous " pointer for the next block to be
      -- translated.
      --
      -- Will be initialized to the header of the genesis block.
    , icPrevBlock :: BlockHeader
    }

-- | Initial interpretation context
--
-- NOTE: In Cardano there is no equivalent of the boot transaction and hence
-- no hash of the boot transaction.
initIntCtxt :: Monad m => DSL.Transaction h Addr -> TranslateT IntException m (IntCtxt h)
initIntCtxt boot = do
    firstSlot <- mapTranslateErrors IntExMkSlot $ translateFirstSlot
    genesis   <- BlockHeaderGenesis <$> translateGenesisHeader
    return $ IntCtxt {
          icLedger    = DSL.ledgerSingleton boot
        , icHashes    = Map.empty
        , icNextSlot  = firstSlot
        , icPrevBlock = genesis
        }

{-------------------------------------------------------------------------------
  The interpretation monad

  NOTE: It is important to limit the scope of 'IntM'. See comments below
  about the instances of 'Interpret' that update the state.
-------------------------------------------------------------------------------}

-- | Interpretation monad
newtype IntT h m a = IntT {
    unIntT :: StateT (IntCtxt h) (TranslateT IntException m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TransCtxt
           , MonadState (IntCtxt h)
           , MonadError IntException
           )

-- | Run the interpreter monad
runIntT :: IntCtxt h
        -> IntT h m a
        -> TranslateT IntException m (a, IntCtxt h)
runIntT ic ma = runStateT (unIntT ma) ic

-- | Run the interpreter monad, given only the boot transaction
runIntBoot :: Monad m
           => DSL.Transaction h Addr
           -> IntT h m a
           -> TranslateT IntException m (a, IntCtxt h)
runIntBoot boot ma = do
    ic <- initIntCtxt boot
    runIntT ic ma

{-------------------------------------------------------------------------------
  Internal monad functions
-------------------------------------------------------------------------------}

liftTranslate :: Monad m
              => (e -> IntException)
              -> ((HasConfiguration, HasUpdateConfiguration) => TranslateT e m a)
              -> IntT h m a
liftTranslate f ta = IntT $ lift $ mapTranslateErrors f $ withConfig $ ta

-- | Add transaction into the context
pushTx :: forall h m. (DSL.Hash h Addr, Monad m)
       => (DSL.Transaction h Addr, TxId) -> IntT h m ()
pushTx (t, id) = modify aux
  where
    aux :: IntCtxt h -> IntCtxt h
    aux ic = IntCtxt {
          icLedger    = DSL.ledgerAdd t            (icLedger ic)
        , icHashes    = Map.insert (DSL.hash t) id (icHashes ic)
        , icNextSlot  = icNextSlot  ic
        , icPrevBlock = icPrevBlock ic
        }

-- | Add a block into the context
--
-- This sets the " previous block " header and increases the next slot number.
pushBlock :: forall m h. Monad m => MainBlock -> IntT h m ()
pushBlock block = do
    s  <- get
    s' <- liftTranslate IntExMkSlot $ aux s
    put s'
  where
    aux :: IntCtxt h -> TranslateT Text m (IntCtxt h)
    aux ic = do
        nextSlot' <- translateNextSlot (icNextSlot ic)
        return IntCtxt {
            icLedger    = icLedger ic
          , icHashes    = icHashes ic
          , icNextSlot  = nextSlot'
          , icPrevBlock = BlockHeaderMain $ block ^. gbHeader
          }

intHash :: Monad m
        => DSL.Hash h Addr => h (DSL.Transaction h Addr) -> IntT h m TxId
intHash h = do
    mId <- Map.lookup h . icHashes <$> get
    case mId of
      Just id -> return id
      Nothing -> throwError $ IntUnknownHash (pretty h)

{-------------------------------------------------------------------------------
  Lift some DSL operations that require a ledger to operations that
  take the IntCtxt
-------------------------------------------------------------------------------}

findHash' :: (DSL.Hash h Addr, Monad m)
          => h (DSL.Transaction h Addr) -> IntT h m (DSL.Transaction h Addr)
findHash' h = (DSL.findHash' h . icLedger) <$> get

inpSpentOutput' :: (DSL.Hash h Addr, Monad m)
                => DSL.Input h Addr -> IntT h m (DSL.Output Addr)
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

  int :: (HasCallStack, Monad m)
      => a -> IntT h m (Interpreted a)

{-------------------------------------------------------------------------------
  Instances that read, but not update, the state
-------------------------------------------------------------------------------}

instance Interpret h Addr where
  type Interpreted Addr = (SomeKeyPair, Address)

  int :: (HasCallStack, Monad m)
      => Addr -> IntT h m (SomeKeyPair, Address)
  int = asks . resolveAddr

instance DSL.Hash h Addr => Interpret h (DSL.Input h Addr) where
  type Interpreted (DSL.Input h Addr) = (TxOwnedInput SomeKeyPair, ResolvedInput)

  int :: (HasCallStack, Monad m)
      => DSL.Input h Addr -> IntT h m (TxOwnedInput SomeKeyPair, ResolvedInput)
  int inp@DSL.Input{..} = do
      -- We figure out who must sign the input by looking at the output
      spentOutput   <- inpSpentOutput' inp
      resolvedInput <- int spentOutput
      isBootstrap   <- isBootstrapTransaction <$> findHash' inpTrans

      if isBootstrap
        then do
          (ownerKey, ownerAddr) <- int $ DSL.outAddr spentOutput
          -- See explanation at 'bootstrapTransaction'
          return ((
                ownerKey
              , TxInUtxo {
                    txInHash  = unsafeHash ownerAddr
                  , txInIndex = 0
                  }
              ), resolvedInput)
        else do
          (ownerKey, _) <- int     $ DSL.outAddr spentOutput
          inpTrans'     <- intHash $ inpTrans
          return ((
                ownerKey
              , TxInUtxo {
                    txInHash  = inpTrans'
                  , txInIndex = inpIndex
                  }
              ), resolvedInput)

instance Interpret h (DSL.Output Addr) where
  type Interpreted (DSL.Output Addr) = TxOutAux

  int :: (HasCallStack, Monad m)
      => DSL.Output Addr -> IntT h m TxOutAux
  int DSL.Output{..} = do
      (_, outAddr') <- int outAddr
      return TxOutAux {
          toaOut = TxOut {
              txOutAddress = outAddr'
            , txOutValue   = mkCoin outVal
            }
        }

instance DSL.Hash h Addr => Interpret h (DSL.Utxo h Addr) where
  type Interpreted (DSL.Utxo h Addr) = Utxo

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Utxo h Addr -> IntT h m Utxo
  int = fmap Map.fromList . mapM aux . DSL.utxoToList
    where
      aux :: (DSL.Input h Addr, DSL.Output Addr)
          -> IntT h m (TxIn, TxOutAux)
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

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Transaction h Addr -> IntT h m RawResolvedTx
  int t = do
      (trIns', resolvedInputs) <- unzip <$> mapM int (DSL.trIns' t)
      trOuts'                  <-           mapM int (DSL.trOuts t)
      txAux   <- liftTranslate IntExClassifyInputs $ mkTx trIns' trOuts'
      pushTx (t, hash (taTx txAux))
      return (txAux, resolvedInputs)
    where
      mkTx :: [TxOwnedInput SomeKeyPair]
           -> [TxOutAux]
           -> TranslateT Text m TxAux
      mkTx inps outs = withConfig $
        case classifyInputs inps of
          Left err ->
            throwError err
          Right (InputsRegular inps') ->
            return . either absurd identity $
              makeMPubKeyTx
                (Right . FakeSigner . regKpSec)
                (NE.fromList inps')
                (NE.fromList outs)
          Right (InputsRedeem (kp, inp)) ->
            return $
              makeRedemptionTx
                (redKpSec kp)
                (NE.fromList [inp])
                (NE.fromList outs)

-- | Interpretation of a list of transactions, oldest first
--
-- Each transaction becomes part of the context for the next.
instance DSL.Hash h Addr => Interpret h (DSL.Block h Addr) where
  type Interpreted (DSL.Block h Addr) = RawResolvedBlock

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Block h Addr -> IntT h m RawResolvedBlock
  int (OldestFirst txs) = do
      (txs', resolvedTxInputs) <- unzip <$> mapM int txs
      prev  <- gets icPrevBlock
      slot  <- gets icNextSlot
      block <- liftTranslate IntExCreateBlock $ mkBlock prev slot txs'
      pushBlock block
      return (block, resolvedTxInputs)
    where
      mkBlock :: BlockHeader -> SlotId -> [TxAux] -> TranslateT Text m MainBlock
      mkBlock prev slotId ts = do
        -- TODO: empty delegation payload
        let dlgPayload = UnsafeDlgPayload []

        -- empty update payload
        let updPayload = def

        -- figure out who needs to sign the block
        BlockSignInfo{..} <- asks $ blockSignInfoForSlot slotId

        withConfig $
          createMainBlockPure
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
  type Interpreted (DSL.Chain h Addr) = OldestFirst [] MainBlock

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Chain h Addr -> IntT h m (OldestFirst [] MainBlock)
  int (OldestFirst blocks) = OldestFirst <$> mapM (fmap fst . int) blocks
