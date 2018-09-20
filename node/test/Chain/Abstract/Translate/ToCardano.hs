{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Translation of an abstract chain into Cardano.
--
-- TODO: Maybe it'd be nice to explain why do we only export instances here.
-- For instance we could point to the `UTxO.Interpreter` module.
module Chain.Abstract.Translate.ToCardano
  (
    -- * Needed so interpretation intstances can be used with type annotations
    Abstract2Cardano
  ) where

import           Universum

import           Control.Lens (ix, (%=), (.=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Data.Default (def)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Wallet.Kernel.Types (RawResolvedBlock,
                     RawResolvedTx (..), mkRawResolvedBlock, mkRawResolvedTx,
                     rawResolvedBlock)
import           Chain.Abstract (Block (..), Chain, Output (..),
                     Transaction (..), hash, outAddr)
import           Pos.Chain.Block (BlockHeader, GenesisBlock, MainBlock)
import qualified Pos.Chain.Block as PosChain (Block)
import           Pos.Chain.Delegation (DlgPayload (UnsafeDlgPayload))
import           Pos.Chain.Ssc (defaultSscPayload)
import           Pos.Chain.Txp (TxAux (..), TxId, TxIn (..), TxOut (..),
                     TxOutAux (..))
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Client.Txp.Util (makeMPubKeyTx, makeRedemptionTx)
import qualified Pos.Core as Core (ProtocolConstants, SlotId (..))
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Common (Coin (..), SlotLeaders, mkCoin)
import           Pos.Core.Slotting (localSlotIndexMaxBound)
import qualified Pos.Crypto (hash)
import           Pos.Crypto.Signing.Safe (SafeSigner (FakeSigner))
import           Pos.DB.Block (RawPayload (..), createMainBlockPure)
import           UTxO.Context (Addr, AddrInfo (..), BlockSignInfo (..),
                     blockSignInfoForSlot, resolveAddr)
import           UTxO.Crypto (ClassifiedInputs (InputsRedeem, InputsRegular),
                     RedeemKeyPair (..), RegularKeyPair (..), SomeKeyPair,
                     TxOwnedInput, classifyInputs)
import qualified UTxO.DSL as DSL (Hash, Input (..), Transaction, Value)
import           UTxO.IntTrans (ConIntT (..), IntCheckpoint (..),
                     IntException (..), IntRollback (..), Interpret (..),
                     Interpretation (..), constants, createEpochBoundary,
                     magic, mkCheckpoint)
import           UTxO.Translate (TranslateT (..), mapTranslateErrors,
                     translateNextSlot, withConfig)


{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

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
--
-- TODO: QUESTION: What do we need the interpretation context for?
data IntCtxt h = IntCtxt {
  -- | Transaction map
  _icTx          :: !(Map (h (DSL.Transaction h Addr)) (TxMeta h))

  -- | Checkpoints
, _icCheckpoints :: !(NonEmpty IntCheckpoint)
}

makeLenses ''IntCtxt

{-------------------------------------------------------------------------------
  The interpretation monad
-------------------------------------------------------------------------------}

-- | Interpretation monad
type IntT = ConIntT IntCtxt

-- | Convenience function to list actions in the 'Translate' monad
--
-- TODO: QUESTION: if the meaning of the type above is the same as:
--
-- > liftTranslateInt
-- >   :: ( Monad m
-- >      , Core.HasConfiguration
-- >      , HasUpdateConfiguration )
-- >   => TranslateT IntException m a -> IntT h e m a
-- >
--
-- I'd rather have the latter, since it involves less nesting and makes it
-- easier to read. But it is just my personal preference.
liftTranslateInt :: Monad m
                 => (   HasUpdateConfiguration
                     => TranslateT IntException m a)
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left $ withConfig $ ta

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
                => (    (HasUpdateConfiguration)
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
        checkpoint <- mkCheckpoint prev slot block (fmap toaOut <$> resolvedTxInputs)
        if isEpochBoundary pc slot
          then second (\ebb -> (raw, Just ebb)) <$> createEpochBoundary checkpoint
          else return (checkpoint, (raw, Nothing))
    where
      unpack :: [RawResolvedTx] -> ([TxAux], [NonEmpty TxOutAux])
      unpack = unzip . map (rawResolvedTx &&& rawResolvedTxInputs)

      isEpochBoundary :: Core.ProtocolConstants -> Core.SlotId -> Bool
      isEpochBoundary pc slot = Core.siSlot slot == localSlotIndexMaxBound pc

      mkBlock :: (HasUpdateConfiguration)
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
        --
        -- TODO: I think we shouldn't use record wild cards as it makes the
        -- code hard to understand (where do the bindings came from), and also
        -- it implicitly introduce bindings that we don't need (which can
        -- conflict with existing names in the current name space).
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

instance DSL.Hash h Addr => Interpret Abstract2Cardano h (Chain h Addr) where
  type Interpreted Abstract2Cardano (Chain h Addr) = OldestFirst [] PosChain.Block

  int :: forall e m. Monad m
      => Chain h Addr -> IntT h e m (OldestFirst [] PosChain.Block)
  int (OldestFirst blocks) =
      OldestFirst . concatMap flatten <$> mapM (int @Abstract2Cardano) blocks
    where
      flatten :: (RawResolvedBlock, Maybe GenesisBlock) -> [PosChain.Block]
      flatten (b, Nothing)  = [Right (rawResolvedBlock b)]
      flatten (b, Just ebb) = [Right (rawResolvedBlock b), Left ebb]


instance Interpret Abstract2Cardano h IntRollback where
  type Interpreted Abstract2Cardano IntRollback = ()

  int :: Monad m => IntRollback -> IntT h e m ()
  int IntRollback = popIntCheckpoint
