-- | ToilT monad transformer. Single-threaded.

module Pos.Txp.Toil.Trans
       ( ToilT
       , runToilTGlobal
       , runToilTLocal
       , execToilTLocal
       , runToilTLocalExtra
       , evalToilTEmpty
       ) where

import           Universum

import           Control.Lens (at, to, (%=), (+=), (.=))
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import qualified Ether

import           Pos.Txp.Toil.Class (MonadStakes (..), MonadStakesRead (..), MonadTxPool (..),
                                     MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types (GenericToilModifier (..), MemPool, ToilModifier, UndoMap,
                                     UtxoModifier, mpLocalTxs, mpSize, svStakes, svTotal, tmMemPool,
                                     tmStakes, tmUndos, tmUtxo)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (ether)

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores ToilModifier and implements
-- writable Toil type classes.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
-- Used for block application now.
type ToilT ext m = Ether.StateT' (GenericToilModifier ext) m

instance MonadUtxoRead m => MonadUtxoRead (ToilT __ m) where
    utxoGet id = ether $ MM.lookupM utxoGet id =<< use tmUtxo

instance MonadUtxoRead m => MonadUtxo (ToilT __ m) where
    utxoPutUnchecked id aux = ether $ tmUtxo %= MM.insert id aux
    utxoDelUnchecked id     = ether $ tmUtxo %= MM.delete id

instance MonadStakesRead m => MonadStakesRead (ToilT __ m) where
    getStake id =
        ether $ (<|>) <$> use (tmStakes . svStakes . at id) <*> getStake id
    getTotalStake =
        ether $ maybe getTotalStake pure =<< use (tmStakes . svTotal)

instance MonadStakesRead m => MonadStakes (ToilT __ m) where
    setStake id c = ether $ tmStakes . svStakes . at id .= Just c

    setTotalStake c = ether $ tmStakes . svTotal .= Just c

instance Monad m => MonadTxPool (ToilT __ m) where
    hasTx id = ether $ use $ tmMemPool . mpLocalTxs . to (HM.member id)

    putTxWithUndo id tx undo = ether $ do
        has <- use $ tmMemPool . mpLocalTxs . to (HM.member id)
        unless has $ do
            tmMemPool . mpLocalTxs . at id .= Just tx
            tmMemPool . mpSize += 1
            tmUndos . at id .= Just undo

    poolSize = ether $ use $ tmMemPool . mpSize

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

-- | Run ToilT using empty modifier. Should be used for global
-- transaction processing.
runToilTGlobal
    :: (Default ext, Functor m)
    => ToilT ext m a -> m (a, GenericToilModifier ext)
runToilTGlobal txpt = Ether.runStateT' txpt def

-- | Run ToilT using empty stakes modifier. Should be used for local
-- transaction processing.
runToilTLocal
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> ToilT () m a
    -> m (a, ToilModifier)
runToilTLocal um mp undo txpt =
    Ether.runStateT' txpt (def {_tmUtxo = um, _tmMemPool = mp, _tmUndos = undo})

evalToilTEmpty
    :: Monad m
    => ToilT () m a
    -> m a
evalToilTEmpty txpt = Ether.evalStateT txpt def

-- | Execute ToilT using empty stakes modifier. Should be used for
-- local transaction processing.
execToilTLocal
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> ToilT () m a
    -> m ToilModifier
execToilTLocal um mp undo = fmap snd . runToilTLocal um mp undo

-- | Like 'runToilTLocal', but takes extra data as argument.
runToilTLocalExtra
    :: (Functor m)
    => UtxoModifier
    -> MemPool
    -> UndoMap
    -> extra
    -> ToilT extra m a
    -> m (a, GenericToilModifier extra)
runToilTLocalExtra um mp undo e =
    flip Ether.runStateT' $
        ToilModifier
        { _tmUtxo = um
        , _tmStakes = def
        , _tmMemPool = mp
        , _tmUndos = undo
        , _tmExtra = e
        }
