-- | Interpreter from the DSL to Cardano types
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
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

import Universum
import Data.Default (def)
import Prelude (Show(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map

import Pos.Block.Logic
import Pos.Client.Txp
import Pos.Core
import Pos.Crypto
import Pos.Ssc (defaultSscPayload)
import Pos.Txp.Toil
import Pos.Update
import Pos.Util.Chrono

import UTxO.Bootstrap
import UTxO.Context
import UTxO.Crypto
import UTxO.Translate
import qualified UTxO.DSL as DSL

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

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Interpretation context
--
-- We need the ledger to resolve hashes, as well as the translation from
-- DSL hashes to Cardano hashes.
data IntCtxt h = IntCtxt {
      icLedger :: DSL.Ledger h Addr
    , icHashes :: Map (h (DSL.Transaction h Addr)) TxId
    }

-- | Initial interpretation context
--
-- NOTE: In Cardano there is no equivalent of the boot transaction and hence
-- no hash of the boot transaction.
initIntCtxt :: DSL.Transaction h Addr -> IntCtxt h
initIntCtxt boot = IntCtxt {
      icLedger = DSL.ledgerSingleton boot
    , icHashes = Map.empty
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

-- | Unwrap the IntM monad stack (internal function)
unIntT' :: IntCtxt h -> IntT h m a -> TranslateT IntException m (a, IntCtxt h)
unIntT' ic ma = runStateT (unIntT ma) ic

-- | Run the interpreter monad
runIntT :: (Interpret h a, Monad m)
        => IntCtxt h
        -> a
        -> TranslateT IntException m (Interpreted a, IntCtxt h)
runIntT ic = unIntT' ic . int

-- | Run the interpreter monad, given only the boot transaction
runIntBoot :: (Interpret h a, Monad m)
           => DSL.Transaction h Addr
           -> a
           -> TranslateT IntException m (Interpreted a, IntCtxt h)
runIntBoot = runIntT . initIntCtxt

{-------------------------------------------------------------------------------
  Internal monad functions
-------------------------------------------------------------------------------}

liftTranslate :: Monad m
              => (e -> IntException)
              -> ((HasConfiguration, HasUpdateConfiguration) => TranslateT e m a)
              -> IntT h m a
liftTranslate f ta = IntT $ lift $ mapTranslateErrors f $ withConfig $ ta

-- | Add transaction into the context
push :: forall h m. (DSL.Hash h Addr, Monad m)
     => (DSL.Transaction h Addr, TxId) -> IntT h m ()
push (t, id) = modify aux
  where
    aux :: IntCtxt h -> IntCtxt h
    aux ic = IntCtxt {
          icLedger = DSL.ledgerAdd t (icLedger ic)
        , icHashes = Map.insert (DSL.hash t) id (icHashes ic)
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
  type Interpreted (DSL.Input h Addr) = TxOwnedInput SomeKeyPair

  int :: (HasCallStack, Monad m)
      => DSL.Input h Addr -> IntT h m (TxOwnedInput SomeKeyPair)
  int inp@DSL.Input{..} = do
      -- We figure out who must sign the input by looking at the output
      spentOutput <- inpSpentOutput' inp
      isBootstrap <- isBootstrapTransaction <$> findHash' inpTrans

      if isBootstrap
        then do
          (ownerKey, ownerAddr) <- int $ DSL.outAddr spentOutput
          -- See explanation at 'bootstrapTransaction'
          return (
                ownerKey
              , TxInUtxo {
                    txInHash  = unsafeHash ownerAddr
                  , txInIndex = 0
                  }
              )
        else do
          (ownerKey, _) <- int     $ DSL.outAddr spentOutput
          inpTrans'     <- intHash $ inpTrans
          return (
                ownerKey
              , TxInUtxo {
                    txInHash  = inpTrans'
                  , txInIndex = inpIndex
                  }
              )

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

-- | Interpretation of transactions
instance DSL.Hash h Addr => Interpret h (DSL.Transaction h Addr) where
  type Interpreted (DSL.Transaction h Addr) = TxAux

  int :: (HasCallStack, Monad m)
      => DSL.Transaction h Addr -> IntT h m TxAux
  int t = do
      trIns'  <- mapM int $ DSL.trIns' t
      trOuts' <- mapM int $ DSL.trOuts t
      liftTranslate IntExClassifyInputs $ case classifyInputs trIns' of
        Left err ->
          throwError err
        Right (InputsRegular trIns'') -> withConfig $
          return . either absurd identity $
          makeMPubKeyTx
            (Right . FakeSigner . regKpSec)
            (NE.fromList trIns'')
            (NE.fromList trOuts')
        Right (InputsRedeem (kp, inp)) -> withConfig $ return $
          makeRedemptionTx
            (redKpSec kp)
            (NE.fromList [inp])
            (NE.fromList trOuts')

{-------------------------------------------------------------------------------
  Instances that change the state

  NOTE: We need to be somewhat careful with using these instances. When
  interpreting a bunch of transactions, those blocks will be part of the
  context of whatever is interpreted next.
-------------------------------------------------------------------------------}

-- | Interpretation of a list of transactions, oldest first
--
-- Each transaction becomes part of the context for the next.
instance DSL.Hash h Addr => Interpret h (DSL.Block h Addr) where
  type Interpreted (DSL.Block h Addr) = OldestFirst [] TxAux

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Block h Addr -> IntT h m (OldestFirst [] TxAux)
  int = fmap OldestFirst . go . toList
    where
      go :: [DSL.Transaction h Addr] -> IntT h m [TxAux]
      go [] = return []
      go (t:ts) = do
          t' <- int t
          push (t, hash (taTx t'))
          (t' :) <$> go ts

-- | Interpretation of a list of list of transactions (basically a chain)
instance DSL.Hash h Addr => Interpret h (DSL.Blocks h Addr) where
  type Interpreted (DSL.Blocks h Addr) = OldestFirst [] (OldestFirst [] TxAux)

  int :: (HasCallStack, Monad m)
      => DSL.Blocks h Addr -> IntT h m (OldestFirst [] (OldestFirst [] TxAux))
  int = mapM int

-- TODO: Here and elsewhere we assume we stay within a single epoch
instance DSL.Hash h Addr => Interpret h (DSL.Chain h Addr) where
  type Interpreted (DSL.Chain h Addr) = OldestFirst [] Block

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Chain h Addr -> IntT h m (OldestFirst [] Block)
  int DSL.Chain{..} = do
      tss <- int chainBlocks
      OldestFirst <$> mkBlocks Nothing 0 (toList (map toList tss))
    where
      mkBlocks :: Maybe MainBlock -> Word16 -> [[TxAux]] -> IntT h m [Block]
      mkBlocks _    _    []       = return []
      mkBlocks prev slot (ts:tss) = do
          lsi <- liftTranslate IntExMkSlot $ mkLocalSlotIndex slot
          let slotId = SlotId (EpochIndex 0) lsi
          block <- mkBlock prev slotId ts
          (Right block :) <$> mkBlocks (Just block) (slot + 1) tss

      mkBlock :: Maybe MainBlock -> SlotId -> [TxAux] -> IntT h m MainBlock
      mkBlock mPrev slotId ts = do
        -- empty delegation payload
        dlgPayload <- liftTranslate IntExMkDlg $ pure (UncheckedDlgPayload [])

        -- empty update payload
        let updPayload = def

        -- previous block header
        -- if none specified, use genesis block
        prev <-
          case mPrev of
            Just prev -> (BlockHeaderMain . view gbHeader) <$> return prev
            Nothing   -> (BlockHeaderGenesis . view gbHeader) <$> asks (ccBlock0 . tcCardano)

        -- figure out who needs to sign the block
        BlockSignInfo{..} <- asks $ blockSignInfoForSlot slotId

        liftTranslate IntExCreateBlock $
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

instance DSL.Hash h Addr => Interpret h (DSL.Utxo h Addr) where
  type Interpreted (DSL.Utxo h Addr) = Utxo

  int :: forall m. (HasCallStack, Monad m)
      => DSL.Utxo h Addr -> IntT h m Utxo
  int = fmap Map.fromList . mapM aux . DSL.utxoToList
    where
      aux :: (DSL.Input h Addr, DSL.Output Addr)
          -> IntT h m (TxIn, TxOutAux)
      aux (inp, out) = do
          (_key, inp') <- int inp
          out'         <- int out
          return (inp', out')
