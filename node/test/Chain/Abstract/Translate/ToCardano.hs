{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Translation of an abstract chain into Cardano.
module Chain.Abstract.Translate.ToCardano (
    IntT
  ) where

import           Cardano.Wallet.Kernel.Types (RawResolvedTx(..), mkRawResolvedTx)
import           Chain.Abstract (Output(..), Transaction(..), hash, outAddr)
import           Control.Lens ((%=), ix)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except
import           Data.Constraint (Dict(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Pos.Chain.Txp (TxAux(..), TxOutAux(..), TxId, TxIn(..), TxOut(..))
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Client.Txp.Util (makeMPubKeyTx, makeRedemptionTx)
import           Pos.Core (HasConfiguration, ProtocolMagic)
import           Pos.Core.Common (Coin(..), mkCoin)
import           Pos.Crypto.Signing.Safe (SafeSigner(SafeSigner, FakeSigner))
import qualified Pos.Crypto (hash)
import           Universum
import           UTxO.Context (Addr, AddrInfo(..), TransCtxt(..), resolveAddr, CardanoContext(..))
import           UTxO.Crypto (SomeKeyPair, TxOwnedInput,
                   ClassifiedInputs(InputsRegular, InputsRedeem), classifyInputs,
                   RegularKeyPair(..), RedeemKeyPair(..))
import qualified UTxO.DSL as DSL (Hash, Input(..), Transaction, Value, outAddr, hash)
import           UTxO.Interpreter (
                   Interpretation(..)
                 , Interpret(..)
                 )

{-------------------------------------------------------------------------------
  Interpretation context
-------------------------------------------------------------------------------}

-- | Checkpoint (we create one for each block we translate)
data IntCheckpoint

-- | Information we keep about the transactions we encounter
data TxMeta h = TxMeta {
  -- | The DSL transaction itself
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
}

makeLenses ''IntCtxt

{-------------------------------------------------------------------------------
  Extract some values we need from the translation context
-------------------------------------------------------------------------------}

magic :: TransCtxt -> ProtocolMagic
magic = ccMagic . tcCardano

{-------------------------------------------------------------------------------
Errors that may occur during interpretation
-------------------------------------------------------------------------------}

-- | Interpretation error
data IntException
  = IntUnknownHash      Text
  | IntExClassifyInputs Text
  | IntIndexOutOfRange  Text Word32 -- ^ During input resolution (hash and index)
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
    , teConfig  :: Dict HasConfiguration
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
           => ((HasConfiguration, HasUpdateConfiguration) => TranslateT e m a)
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
                 => (   (HasConfiguration, HasUpdateConfiguration)
                     => TranslateT IntException m a)
                 -> IntT h e m a
liftTranslateInt ta =  IntT $ lift $ mapTranslateErrors Left $ withConfig $ ta

{-------------------------------------------------------------------------------
  Dealing with the transactions
-------------------------------------------------------------------------------}

-- | Add transaction into the context
putTx :: (DSL.Hash h Addr, Monad m)
      => Transaction h Addr
      -> TxId
      -> IntT h e m ()
putTx t txId = icTx %= Map.insert (hash t) TxMeta {
  tmTx   = t
, tmHash = txId
}

-- | Get transaction from the context
getTx :: (DSL.Hash h Addr, Monad m)
      => h (DSL.Transaction h Addr)
      -> IntT h e m (TxMeta h)
getTx h = do
    txMap <- use icTx
    case Map.lookup h txMap of
      Nothing     -> throwError $ Left $ IntUnknownHash (pretty h)
      Just txMeta -> return txMeta

-- | Lookup the Cardano hash for the given DSL hash
intHash :: (Monad m, DSL.Hash h Addr)
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
