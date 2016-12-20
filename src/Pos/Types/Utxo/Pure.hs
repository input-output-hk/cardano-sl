{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

-- | Pure version of UTXO.

module Pos.Types.Utxo.Pure
       ( UtxoReaderT (..)
       , runUtxoReaderT

       , UtxoReader
       , runUtxoReader

       , UtxoStateT (..)
       , runUtxoStateT
       , evalUtxoStateT
       , execUtxoStateT

       , UtxoState
       , runUtxoState
       , evalUtxoState
       , execUtxoState

       , applyTxToUtxoPure
       , applyTxToUtxoPure'
       , normalizeTxsPure
       , normalizeTxsPure'
       , verifyAndApplyTxsOldPure
       , verifyAndApplyTxsOldPure'
       , verifyTxUtxoPure
       ) where

import           Control.Lens             (at, over, use, view, (.=), _1)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.Trans      (MonadTrans (..))
import           Data.List                ((\\))
import           Serokell.Util.Verify     (VerificationRes (..))
import           Universum

import           Pos.Binary.Types         ()
import           Pos.Crypto               (WithHash (..))
import           Pos.Types.Types          (IdTxWitness, Tx, TxIn (..), TxWitness, Utxo)
import           Pos.Types.Utxo.Class     (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Types.Utxo.Functions (applyTxToUtxo, applyTxToUtxo', convertFrom',
                                           convertTo', verifyAndApplyTxsOld,
                                           verifyAndApplyTxsOld', verifyTxUtxo)

----------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------

newtype UtxoReaderT m a = UtxoReaderT
    { getUtxoReaderT :: ReaderT Utxo m a
    } deriving (Functor, Applicative, Monad, MonadReader Utxo)

instance Monad m => MonadUtxoRead (UtxoReaderT m) where
    utxoGet TxIn {..} = UtxoReaderT $ view $ at (txInHash, txInIndex)

instance MonadTrans UtxoReaderT where
    lift = UtxoReaderT . lift

runUtxoReaderT :: UtxoReaderT m a -> Utxo -> m a
runUtxoReaderT = runReaderT . getUtxoReaderT

type UtxoReader = UtxoReaderT Identity

runUtxoReader :: UtxoReader a -> Utxo -> a
runUtxoReader r = runIdentity . runUtxoReaderT r

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

newtype UtxoStateT m a = UtxoStateT
    { getUtxoStateT :: StateT Utxo m a
    } deriving (Functor, Applicative, Monad, MonadState Utxo)

instance Monad m => MonadUtxoRead (UtxoStateT m) where
    utxoGet TxIn {..} = UtxoStateT $ use $ at (txInHash, txInIndex)

instance Monad m => MonadUtxo (UtxoStateT m) where
    utxoPut TxIn {..} v = UtxoStateT $ at (txInHash, txInIndex) .= Just v
    utxoDel TxIn {..} = UtxoStateT $ at (txInHash, txInIndex) .= Nothing

instance MonadTrans UtxoStateT where
    lift = UtxoStateT . lift

runUtxoStateT :: UtxoStateT m a -> Utxo -> m (a, Utxo)
runUtxoStateT = runStateT . getUtxoStateT

evalUtxoStateT :: Monad m => UtxoStateT m a -> Utxo -> m a
evalUtxoStateT = evalStateT . getUtxoStateT

execUtxoStateT :: Monad m => UtxoStateT m a -> Utxo -> m Utxo
execUtxoStateT = execStateT . getUtxoStateT

type UtxoState = UtxoStateT Identity

runUtxoState :: UtxoState a -> Utxo -> (a, Utxo)
runUtxoState r = runIdentity . runUtxoStateT r

evalUtxoState :: UtxoState a -> Utxo -> a
evalUtxoState r = runIdentity . evalUtxoStateT r

execUtxoState :: UtxoState a -> Utxo -> Utxo
execUtxoState r = runIdentity . execUtxoStateT r

----------------------------------------------------------------------------
-- Pure versions of functions
----------------------------------------------------------------------------

-- | Pure version of applyTxToUtxo.
applyTxToUtxoPure :: WithHash Tx -> Utxo -> Utxo
applyTxToUtxoPure tx = execUtxoState $ applyTxToUtxo tx

-- | Pure version of applyTxToUtxo'.
applyTxToUtxoPure' :: IdTxWitness -> Utxo -> Utxo
applyTxToUtxoPure' w = execUtxoState $ applyTxToUtxo' w

-- CHECK: @TxUtxoPure
-- #verifyTxUtxo

-- | Pure version of verifyTxUtxo.
verifyTxUtxoPure :: Utxo -> (Tx, TxWitness) -> VerificationRes
verifyTxUtxoPure utxo txw = runUtxoReader (verifyTxUtxo txw) utxo

-- CHECK: @verifyAndApplyTxsOldPure
-- #verifyAndApplyTxsOld
verifyAndApplyTxsOldPure
    :: [(WithHash Tx, TxWitness)]
    -> Utxo
    -> Either Text ([(WithHash Tx, TxWitness)], Utxo)
verifyAndApplyTxsOldPure txws utxo =
    let (res, newUtxo) = runUtxoState (verifyAndApplyTxsOld txws) utxo
    in (, newUtxo) <$> res

-- CHECK: @verifyAndApplyTxsOldPure'
-- #verifyAndApplyTxsOld'
verifyAndApplyTxsOldPure' :: [IdTxWitness] -> Utxo -> Either Text ([IdTxWitness], Utxo)
verifyAndApplyTxsOldPure' txws utxo =
    let (res, newUtxo) = runUtxoState (verifyAndApplyTxsOld' txws) utxo
    in (, newUtxo) <$> res

-- CHECK: @normalizeTxsPure
-- | Takes the set of transactions and utxo, returns only those
-- transactions that can be applied inside. Bonus -- returns them
-- sorted (topographically).
normalizeTxsPure :: [(WithHash Tx, TxWitness)] -> Utxo -> [(WithHash Tx, TxWitness)]
normalizeTxsPure = normGo []
  where
    -- checks if transaction can be applied, adds it to first arg and
    -- to utxo if ok, does nothing otherwise
    canApply :: (WithHash Tx, TxWitness)
             -> ([(WithHash Tx, TxWitness)], Utxo)
             -> ([(WithHash Tx, TxWitness)], Utxo)
    canApply txw prev@(txws, utxo) =
        case verifyTxUtxoPure utxo (over _1 whData txw) of
            VerFailure _ -> prev
            VerSuccess   -> (txw : txws, fst txw `applyTxToUtxoPure` utxo)

    normGo :: [(WithHash Tx, TxWitness)]
           -> [(WithHash Tx, TxWitness)]
           -> Utxo
           -> [(WithHash Tx, TxWitness)]
    normGo result pending curUtxo =
        let !(!canBeApplied, !newUtxo) = foldr' canApply ([], curUtxo) pending
            newPending = pending \\ canBeApplied
            newResult = result ++ canBeApplied
        in if null canBeApplied
               then result
               else normGo newResult newPending newUtxo

-- CHECK: @normalizeTxsPure'
-- #normalizeTxsPure
normalizeTxsPure' :: [IdTxWitness] -> Utxo -> [IdTxWitness]
normalizeTxsPure' tx utxo =
    let converted = convertTo' tx in
    convertFrom' $ normalizeTxsPure converted utxo
