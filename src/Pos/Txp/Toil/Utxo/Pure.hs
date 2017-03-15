{-# LANGUAGE CPP #-}

-- | Pure version of UTXO.

module Pos.Txp.Toil.Utxo.Pure
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
       , verifyTxUtxoPure
       ) where

import           Control.Lens                (at, (.=))
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.Trans         (MonadTrans (..))
import           Universum

import           Pos.Binary.Core             ()
import           Pos.Crypto                  (WithHash (..))
import           Pos.Txp.Core                (Tx, TxAux, TxDistribution, TxUndo)
import           Pos.Txp.Toil.Class          (MonadToilEnv, MonadUtxo (..),
                                              MonadUtxoRead (..))
import           Pos.Txp.Toil.Failure        (ToilVerFailure)
import           Pos.Txp.Toil.Types          (Utxo)
import           Pos.Txp.Toil.Utxo.Functions (VTxContext, applyTxToUtxo, verifyTxUtxo)
#ifdef WITH_EXPLORER
import           Pos.Txp.Toil.Class          (MonadTxExtraRead (..))
import           Pos.Util                    (NewestFirst (..))
#endif

----------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------

newtype UtxoReaderT m a = UtxoReaderT
    { getUtxoReaderT :: ReaderT Utxo m a
    } deriving (Functor, Applicative, Monad, MonadReader Utxo, MonadError e, MonadToilEnv)

instance Monad m => MonadUtxoRead (UtxoReaderT m) where
    utxoGet id = UtxoReaderT $ view $ at id

#ifdef WITH_EXPLORER
instance Monad m => MonadTxExtraRead (UtxoReaderT m) where
    getTxExtra _ = pure Nothing
    getAddrHistory _ = pure $ NewestFirst []
#endif

instance MonadTrans UtxoReaderT where
    lift = UtxoReaderT . lift

runUtxoReaderT :: UtxoReaderT m a -> Utxo -> m a
runUtxoReaderT = runReaderT . getUtxoReaderT

type UtxoReader = UtxoReaderT Identity

runUtxoReader :: UtxoReader a -> Utxo -> a
runUtxoReader r = runIdentity . runUtxoReaderT r

instance MonadUtxoRead ((->) Utxo) where
    utxoGet txIn utxo = utxo ^. at txIn

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

newtype UtxoStateT m a = UtxoStateT
    { getUtxoStateT :: StateT Utxo m a
    } deriving (Functor, Applicative, Monad, MonadState Utxo)

instance Monad m => MonadUtxoRead (UtxoStateT m) where
    utxoGet id = UtxoStateT $ use $ at id

instance Monad m => MonadUtxo (UtxoStateT m) where
    utxoPut id v = UtxoStateT $ at id .= Just v
    utxoDel id = UtxoStateT $ at id .= Nothing

#ifdef WITH_EXPLORER
instance Monad m => MonadTxExtraRead (UtxoStateT m) where
    getTxExtra _ = pure Nothing
    getAddrHistory _ = pure $ NewestFirst []
#endif

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

-- | Pure version of verifyTxUtxo.
verifyTxUtxoPure
    :: MonadError ToilVerFailure m
    => VTxContext -> Utxo -> TxAux -> m TxUndo
verifyTxUtxoPure ctx utxo txAux = runUtxoReaderT (verifyTxUtxo ctx txAux) utxo

-- | Pure version of applyTxToUtxo.
applyTxToUtxoPure :: WithHash Tx -> TxDistribution -> Utxo -> Utxo
applyTxToUtxoPure tx d = execUtxoState $ applyTxToUtxo tx d
