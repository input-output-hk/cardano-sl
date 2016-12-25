{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

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

import           Control.Lens             (at, over, use, view, (.=), (^.), _1, _3)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.Trans      (MonadTrans (..))
import           Data.List                ((\\))
import           Serokell.Util.Verify     (VerificationRes (..))
import           Universum

import           Pos.Binary.Types         ()
import           Pos.Crypto               (WithHash (..))
import           Pos.Types.Types          (Tx, TxAux, TxDistribution, TxId, TxIn (..),
                                           TxWitness, Utxo)
import           Pos.Types.Utxo.Class     (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Types.Utxo.Functions (applyTxToUtxo, applyTxToUtxo', convertFrom',
                                           convertTo', verifyAndApplyTxsOld,
                                           verifyAndApplyTxsOld', verifyTxUtxo)
import           Pos.Util                 (eitherToVerRes)
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
applyTxToUtxoPure :: WithHash Tx -> TxDistribution -> Utxo -> Utxo
applyTxToUtxoPure tx d = execUtxoState $ applyTxToUtxo tx d

-- | Pure version of applyTxToUtxo'.
applyTxToUtxoPure' :: (TxId, TxAux) -> Utxo -> Utxo
applyTxToUtxoPure' w = execUtxoState $ applyTxToUtxo' w

-- CHECK: @TxUtxoPure
-- #verifyTxUtxo

-- | Pure version of verifyTxUtxo.
verifyTxUtxoPure :: Bool -> Utxo -> TxAux -> VerificationRes
verifyTxUtxoPure verifyAlone utxo txw =
    eitherToVerRes $ runUtxoReader (verifyTxUtxo verifyAlone txw) utxo

-- CHECK: @verifyAndApplyTxsOldPure
-- #verifyAndApplyTxsOld
verifyAndApplyTxsOldPure
    :: [(WithHash Tx, TxWitness, TxDistribution)]
    -> Utxo
    -> Either Text ([(WithHash Tx, TxWitness, TxDistribution)], Utxo)
verifyAndApplyTxsOldPure txws utxo =
    let (res, newUtxo) = runUtxoState (verifyAndApplyTxsOld txws) utxo
    in (, newUtxo) <$> res

-- CHECK: @verifyAndApplyTxsOldPure'
-- #verifyAndApplyTxsOld'
verifyAndApplyTxsOldPure' :: [(TxId, TxAux)] -> Utxo -> Either Text ([(TxId, TxAux)], Utxo)
verifyAndApplyTxsOldPure' txws utxo =
    let (res, newUtxo) = runUtxoState (verifyAndApplyTxsOld' txws) utxo
    in (, newUtxo) <$> res

-- CHECK: @normalizeTxsPure
-- | Takes the set of transactions and utxo, returns only those
-- transactions that can be applied inside. Bonus -- returns them
-- sorted (topographically).
normalizeTxsPure :: [(WithHash Tx, TxWitness, TxDistribution)] -> Utxo -> [(WithHash Tx, TxWitness, TxDistribution)]
normalizeTxsPure = normGo []
  where
    -- checks if transaction can be applied, adds it to first arg and
    -- to utxo if ok, does nothing otherwise
    canApply :: (WithHash Tx, TxWitness, TxDistribution)
             -> ([(WithHash Tx, TxWitness, TxDistribution)], Utxo)
             -> ([(WithHash Tx, TxWitness, TxDistribution)], Utxo)
    canApply txw prev@(txws, utxo) =
        case verifyTxUtxoPure True utxo (over _1 whData txw) of
            VerFailure _ -> prev
            VerSuccess   -> (txw : txws, applyTxToUtxoPure (txw^._1) (txw^._3) utxo)

    normGo :: [(WithHash Tx, TxWitness, TxDistribution)]
           -> [(WithHash Tx, TxWitness, TxDistribution)]
           -> Utxo
           -> [(WithHash Tx, TxWitness, TxDistribution)]
    normGo result pending curUtxo =
        let !(!canBeApplied, !newUtxo) = foldr' canApply ([], curUtxo) pending
            newPending = pending \\ canBeApplied
            newResult = result ++ canBeApplied
        in if null canBeApplied
               then result
               else normGo newResult newPending newUtxo

-- CHECK: @normalizeTxsPure'
-- #normalizeTxsPure
normalizeTxsPure' :: [(TxId, TxAux)] -> Utxo -> [(TxId, TxAux)]
normalizeTxsPure' tx utxo =
    let converted = convertTo' tx in
    convertFrom' $ normalizeTxsPure converted utxo
