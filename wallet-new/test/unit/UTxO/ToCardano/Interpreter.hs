{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Interpreter from the DSL to Cardano types
module UTxO.ToCardano.Interpreter (
    -- * Interpretation context
    IntCtxt -- opaque
  , initIntCtxt
    -- * Interpretation monad
  , IntT
  , runIntT
  , runIntT'
  , runIntBoot
  , runIntBoot'
    -- * Interpreter proper
  , DSL2Cardano
  , popIntCheckpoint
  , pushCheckpoint
    -- * Re-exported
  , IntException (..)
  , Interpret(..)
  , IntRollback(..)
  , IntSwitchToFork(..)
    -- * DSL BlockMeta'
  , BlockMeta'(..)
  ) where

import           Universum hiding (id)

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (boundAddrAttrSize, boundTxAttrSize)
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta,
                     BlockMeta (..))
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved
import           Cardano.Wallet.Kernel.Types
import           Control.Arrow ((&&&))
import           Control.Lens (strict, (%=), (.=))
import           Control.Lens.TH (makeLenses)
import           Data.Default (def)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Time.Units (fromMicroseconds)
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Pos.Binary.Class (biSize)
import           Pos.Chain.Block (Block, BlockHeader (..), GenesisBlock,
                     MainBlock, headerHash)
import           Pos.Chain.Delegation (DlgPayload (..))
import           Pos.Chain.Ssc (defaultSscPayload)
import           Pos.Chain.Txp (TxAux (..), TxId, TxIn (..), TxOut (..),
                     TxOutAux (..), Utxo, txAttributes)
import           Pos.Chain.Update (HasUpdateConfiguration, updateConfiguration)
import           Pos.Client.Txp
import           Pos.Core
import           Pos.Core.Chrono
import           Pos.Crypto
import           Pos.DB.Block (RawPayload (..), createMainBlockPure)
import           Serokell.Util (listJson)
import           Test.Pos.Chain.Genesis.Dummy (dummyConfig, dummyEpochSlots,
                     dummyK)
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.Crypto
import qualified UTxO.DSL as DSL
import           UTxO.IntTrans (ConIntT (..), IntCheckpoint (..),
                     IntException (..), IntRollback (..), Interpret (..),
                     Interpretation (..), createEpochBoundary, magic,
                     mkCheckpoint)
import           UTxO.Translate
import           UTxO.Util (at)

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
            , icMainBlockHdr  = Nothing
            , icPrevMainHH    = Nothing
            , icEpochLeaders  = leaders
            , icStakes        = initStakes
            , icCrucialStakes = initStakes
            } :| []
        }

{-------------------------------------------------------------------------------
  The interpretation monad

  NOTE: It is important to limit the scope of 'IntM'. See comments below
  about the instances of 'Interpret' that update the state.
-------------------------------------------------------------------------------}

type IntT = ConIntT IntCtxt

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
                 => (HasUpdateConfiguration => TranslateT IntException m a)
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
                => (    HasUpdateConfiguration
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
  Translate the DSL UTxO definitions to Cardano types

  NOTE: Delegation in Cardano is described in the document
  "Delegation and Stake Locking in Cardano SL"
  <cardano-sl-articles/delegation/pdf/article.pdf>.
-------------------------------------------------------------------------------}

data DSL2Cardano

instance Interpretation DSL2Cardano where
  type IntCtx DSL2Cardano = IntT

{-------------------------------------------------------------------------------
  Instances that read, but not update, the state
-------------------------------------------------------------------------------}

instance Interpret DSL2Cardano h DSL.Value where
  type Interpreted DSL2Cardano DSL.Value = Coin

  int :: Monad m => DSL.Value -> IntT h e m Coin
  int = return . mkCoin

instance Interpret DSL2Cardano h Addr where
  type Interpreted DSL2Cardano Addr = AddrInfo

  int :: Monad m => Addr -> IntT h e m AddrInfo
  int = asks . resolveAddr

instance DSL.Hash h Addr => Interpret DSL2Cardano h (DSL.Input h Addr) where
  type Interpreted DSL2Cardano (DSL.Input h Addr) = (TxOwnedInput SomeKeyPair, ResolvedInput)

  int :: Monad m
      => DSL.Input h Addr -> IntT h e m (TxOwnedInput SomeKeyPair, ResolvedInput)
  int inp@DSL.Input{..} = do
      -- We figure out who must sign the input by looking at the output
      spentOutput   <- inpSpentOutput' inp
      resolvedInput <- (int @DSL2Cardano) spentOutput
      isBootstrap   <- isBootstrapTransaction <$> findHash' inpTrans

      if isBootstrap
        then do
          AddrInfo{..} <- (int @DSL2Cardano) $ DSL.outAddr spentOutput
          -- See explanation at 'bootstrapTransaction'
          return (
                   (addrInfoAddrKey, TxInUtxo (unsafeHash addrInfoCardano) 0)
                 , resolvedInput
                 )
        else do
          AddrInfo{..} <- (int @DSL2Cardano) $ DSL.outAddr spentOutput
          inpTrans'    <- intHash $ inpTrans
          return (
                   (addrInfoAddrKey, TxInUtxo inpTrans' inpIndex)
                 , resolvedInput
                 )

instance Interpret DSL2Cardano h (DSL.Output h Addr) where
  type Interpreted DSL2Cardano (DSL.Output h Addr) = TxOutAux

  int :: Monad m
      => DSL.Output h Addr -> IntT h e m TxOutAux
  int DSL.Output{..} = do
      AddrInfo{..} <- (int @DSL2Cardano) outAddr
      outVal'      <- (int @DSL2Cardano) outVal
      let addrAttrSize = biSize (addrAttributes addrInfoCardano)
      unless (addrAttrSize <= boundAddrAttrSize) $
        throwError $ Left $ IntAddrAttrBoundsExceeded addrAttrSize

      return TxOutAux {
          toaOut = TxOut {
              txOutAddress = addrInfoCardano
            , txOutValue   = outVal'
            }
        }

instance DSL.Hash h Addr => Interpret DSL2Cardano h (DSL.Utxo h Addr) where
  type Interpreted DSL2Cardano (DSL.Utxo h Addr) = Utxo

  int :: forall e m. Monad m
      => DSL.Utxo h Addr -> IntT h e m Utxo
  int = fmap Map.fromList . mapM aux . DSL.utxoToList
    where
      aux :: (DSL.Input h Addr, DSL.Output h Addr)
          -> IntT h e m (TxIn, TxOutAux)
      aux (inp, out) = do
          ((_key, inp'), _) <- (int @DSL2Cardano) inp
          out'              <- (int @DSL2Cardano) out
          return (inp', out')

{-------------------------------------------------------------------------------
  (included in "Instances that read, but not update, the state")

  Block metadata: defines a DSL `BlockMeta'` analogy to the Cardano `BlockMeta`
  along with an interpreter instance to translate `BlockMeta'` to `BlockMeta`
-------------------------------------------------------------------------------}

-- | Block metadata
--
-- Models the Cardano BlockMeta, replacing the map key types:
-- * replace Cardano\TxId with DSL\TxId
-- * replace Cardano\Address with DSL\Addr
data BlockMeta' h = BlockMeta' {
      -- | SlotIds for all confirmed transactions
      _blockMetaSlotId'      :: Map (h (DSL.Transaction h Addr)) SlotId

    , -- | Address metadata using the DSL `Addr` and the Cardano `AddressMeta`
      _blockMetaAddressMeta' :: Map Addr AddressMeta
    }

-- | Interpretation of block metadata
--
-- NOTE: BlockMeta' has a mix of DSL and Cardano values, only the DSL values are translated
instance DSL.Hash h Addr => Interpret DSL2Cardano h (BlockMeta' h) where
  type Interpreted DSL2Cardano (BlockMeta' h) = BlockMeta

  int :: forall e m. Monad m
      => BlockMeta' h -> IntT h e m BlockMeta
  int (BlockMeta' txs' addrMeta') = do
      _blockMetaSlotId      <- intTxIds txs'
      _blockMetaAddressMeta <- intAddrMetas addrMeta'
      return $ BlockMeta {..}
      where
          intAddrMetas :: Map Addr AddressMeta -> IntT h e m (Map (InDb Address) AddressMeta)
          intAddrMetas addrMetas= Map.fromList <$> mapM intAddrMeta (Map.toList addrMetas)

          -- Interpret only the key, leaving the indexed value AddressMeta unchanged
          intAddrMeta :: (Addr,AddressMeta) -> IntT h e m (InDb Address, AddressMeta)
          intAddrMeta (addr,addrMeta) = (,addrMeta) . InDb . addrInfoCardano <$> (int @DSL2Cardano) addr

          intTxIds :: Map (h (DSL.Transaction h Addr)) SlotId -> IntT h e m (InDb (Map TxId SlotId))
          intTxIds txIds = InDb . Map.fromList <$> mapM intTxId (Map.toList txIds)

          -- Interpret only the key, leaving the indexed value SlotId unchanged
          intTxId :: (h (DSL.Transaction h Addr),SlotId) -> IntT h e m (TxId, SlotId)
          intTxId (txId,slotId) = (,slotId) <$> intHash txId

{-------------------------------------------------------------------------------
  Instances that change the state

  NOTE: We need to be somewhat careful with using these instances. When
  interpreting a bunch of transactions, those blocks will be part of the
  context of whatever is interpreted next.
-------------------------------------------------------------------------------}

-- | Interpretation of transactions
instance DSL.Hash h Addr => Interpret DSL2Cardano h (DSL.Transaction h Addr) where
  type Interpreted DSL2Cardano (DSL.Transaction h Addr) = RawResolvedTx

  int :: forall e m. Monad m
      => DSL.Transaction h Addr -> IntT h e m RawResolvedTx
  int t = do
      let currentTime = getSomeTimestamp -- Pos.Core.Timestamp $ minBound
      (trIns', resolvedInputs) <- unzip <$> mapM (int @DSL2Cardano) (DSL.trIns' t)
      trOuts'                  <-           mapM (int @DSL2Cardano) (DSL.trOuts t)
      txAux   <- liftTranslateInt $ mkTx trIns' trOuts'
      let txAttrSize = biSize (taTx txAux ^. txAttributes)
      unless (txAttrSize <= boundTxAttrSize) $
        throwError $ Left $ IntTxAttrBoundsExceeded txAttrSize
      putTxMeta t $ hash (taTx txAux)
      return $ mkRawResolvedTx currentTime txAux (NE.fromList resolvedInputs)
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
instance DSL.Hash h Addr => Interpret DSL2Cardano h (DSL.Block h Addr) where
  -- The block and the EBB, if any
  type Interpreted DSL2Cardano (DSL.Block h Addr) = (RawResolvedBlock, Maybe GenesisBlock)

  int :: forall e m. Monad m
      => DSL.Block h Addr -> IntT h e m (RawResolvedBlock, Maybe GenesisBlock)
  int (OldestFirst txs) = do
      (txs', resolvedTxInputs) <- unpack <$> mapM (int @DSL2Cardano) txs
      pushCheckpoint $ \prev slot -> do
        block <- mkBlock
                   (icEpochLeaders prev)
                   (icBlockHeader  prev)
                   slot
                   txs'
        let currentTime = getSomeTimestamp
        let ctxt = BlockContext {
                       _bcSlotId   = InDb  $  slot
                     , _bcHash     = InDb  $  headerHash block
                     , _bcPrevMain = view strict $ InDb <$> icMainBlockHdr prev
                     }
        let raw = mkRawResolvedBlock block resolvedTxInputs currentTime ctxt
        checkpoint <- mkCheckpoint prev slot block (fmap toaOut <$> resolvedTxInputs)
        if isEpochBoundary slot
          then second (\ebb -> (raw, Just ebb)) <$> createEpochBoundary checkpoint
          else return (checkpoint, (raw, Nothing))
    where
      unpack :: [RawResolvedTx] -> ([TxAux], [ResolvedTxInputs])
      unpack = unzip . map (rawResolvedTx &&& rawResolvedTxInputs)

      isEpochBoundary :: SlotId -> Bool
      isEpochBoundary slot = siSlot slot == localSlotIndexMaxBound dummyEpochSlots

      mkBlock :: HasUpdateConfiguration
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

        flip runReaderT updateConfiguration $ createMainBlockPure
          dummyConfig
          blockSizeLimit
          prev
          (Just (bsiPSK, bsiLeader))
          slotId
          bsiKey
          (RawPayload
              (toList ts)
              (defaultSscPayload dummyK (siSlot slotId)) -- TODO
              dlgPayload
              updPayload
            )

      -- TODO: Get this value from somewhere rather than hardcoding it
      blockSizeLimit = 2 * 1024 * 1024 -- 2 MB

instance DSL.Hash h Addr => Interpret DSL2Cardano h (DSL.Chain h Addr) where
  type Interpreted DSL2Cardano (DSL.Chain h Addr) = OldestFirst [] Block

  int :: forall e m. Monad m
      => DSL.Chain h Addr -> IntT h e m (OldestFirst [] Block)
  int (OldestFirst blocks) =
      OldestFirst . concatMap flatten <$> mapM (int @DSL2Cardano) blocks
    where
      flatten :: (RawResolvedBlock, Maybe GenesisBlock) -> [Block]
      flatten (b, Nothing)  = [Right (rawResolvedBlock b)]
      flatten (b, Just ebb) = [Right (rawResolvedBlock b), Left ebb]

instance Interpret DSL2Cardano h IntRollback where
  type Interpreted DSL2Cardano IntRollback = ()

  int :: Monad m => IntRollback -> IntT h e m ()
  int IntRollback = popIntCheckpoint

-- | Like 'IntRollback', but corresponding to a switch-to-fork event
data IntSwitchToFork h = IntSwitchToFork Int (DSL.Chain h Addr)

-- | When we interpret 'IntSwitchToFork' we ignore EBBs
instance DSL.Hash h Addr => Interpret DSL2Cardano h (IntSwitchToFork h) where
  type Interpreted DSL2Cardano (IntSwitchToFork h) = OldestFirst [] RawResolvedBlock

  int (IntSwitchToFork 0 (OldestFirst blocks)) = do
      OldestFirst . map fst <$> mapM (int @DSL2Cardano) blocks
  int (IntSwitchToFork n bs) = do
      int @DSL2Cardano $ IntRollback
      int @DSL2Cardano $ IntSwitchToFork (n - 1) bs

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeLeft :: Either a Void -> a
mustBeLeft (Left  a) = a
mustBeLeft (Right b) = absurd b

getSomeTimestamp :: Pos.Core.Timestamp
getSomeTimestamp = Pos.Core.Timestamp $ fromMicroseconds 12340000


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
