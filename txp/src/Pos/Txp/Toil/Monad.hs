{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

-- | Some monads used in Toil and primitive actions.

module Pos.Txp.Toil.Monad
       (
         -- * Monadic Utxo
         UtxoM
       , runUtxoM
       , evalUtxoM
       , execUtxoM
       , utxoGet
       , utxoPut
       , utxoDel

         -- * Monadic local Toil
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo
       , ExtendedLocalToilM
       , extendLocalToilM

         -- * Monadic global Toil
       , StakesLookupF
       , GlobalToilState (..)
       , gtsUtxoModifier
       , gtsStakesView
       , defGlobalToilState
       , GlobalToilEnv (..)
       , GlobalToilMBase
       , GlobalToilM
       , runGlobalToilMBase
       , runGlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake
       , ExtendedGlobalToilM
       , extendGlobalToilM

         -- * Conversions
       , utxoMToLocalToilM
       , utxoMToGlobalToilM
       ) where

import           Universum hiding (id)

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Free.Church (F (..), foldF)
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import           Fmt ((+|), (|+))
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Types (MemPool, StakesView, UndoMap, UtxoLookup, UtxoModifier,
                                     mpLocalTxs, mpSize, svStakes, svTotal)
import           Pos.Util (type (~>))
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Monadic actions with Utxo.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup values in UTXO and modify it.
type UtxoM = ReaderT UtxoLookup (State UtxoModifier)

-- | Run 'UtxoM' action using 'UtxoLookup' and 'UtxoModifier'.
runUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> (a, UtxoModifier)
runUtxoM modifier getter = usingState modifier . usingReaderT getter

-- | Version of 'runUtxoM' which discards final state.
evalUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> a
evalUtxoM = fst ... runUtxoM

-- | Version of 'runUtxoM' which discards action's result.
execUtxoM :: UtxoModifier -> UtxoLookup -> UtxoM a -> UtxoModifier
execUtxoM = snd ... runUtxoM

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: TxIn -> UtxoM (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- ask
    MM.lookup utxoLookup txIn <$> use identity

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: TxIn -> TxOutAux -> UtxoM ()
utxoPut id txOut = utxoGet id >>= \case
    Nothing -> identity %= MM.insert id txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel id = utxoGet id >>= \case
    Just _  -> identity %= MM.delete id
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|id|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

-- | Mutable state used in local Toil.
data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    }

makeLenses ''LocalToilState

-- | Monad in which local Toil happens.
type LocalToilM = ReaderT UtxoLookup (State LocalToilState)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: TxId -> LocalToilM Bool
hasTx id = isJust <$> use (ltsMemPool . mpLocalTxs . at id)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: TxId -> TxAux -> TxUndo -> LocalToilM ()
putTxWithUndo id tx undo =
    unlessM (hasTx id) $ do
        ltsMemPool . mpLocalTxs . at id .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at id .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: LocalToilM Int
memPoolSize = use $ ltsMemPool . mpSize

-- | Extended version of 'LocalToilM'. It allows to put extra data
-- into reader context, extra state and also adds logging
-- capabilities. It's needed for explorer which has more complicated
-- transaction processing.
type ExtendedLocalToilM extraEnv extraState =
    ReaderT (UtxoLookup, extraEnv) (
        StateT (LocalToilState, extraState) (
            NamedPureLogger Identity
    ))

-- | Natural transformation from 'LocalToilM to 'ExtendedLocalToilM'.
extendLocalToilM :: LocalToilM a -> ExtendedLocalToilM extraEnv extraState a
extendLocalToilM = mapReaderT (mapStateT lift . zoom _1) . magnify _1

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

-- | Type which parameterizes free monad with access to Stakes.
data StakesLookupF a =
    StakesLookupF StakeholderId
                  (Maybe Coin -> a)
    deriving (Functor)

-- | Mutable state used in global Toil.
data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    }

-- | Default 'GlobalToilState'.
defGlobalToilState :: GlobalToilState
defGlobalToilState =
    GlobalToilState {_gtsUtxoModifier = mempty, _gtsStakesView = def}

makeLenses ''GlobalToilState

-- | Immutable environment used in global Toil.
data GlobalToilEnv = GlobalToilEnv
    { _gteUtxo       :: !UtxoLookup
    , _gteTotalStake :: !Coin
    }

makeLenses ''GlobalToilEnv

-- | Base monad in which global Toil happens.
type GlobalToilMBase = NamedPureLogger (F StakesLookupF)

-- | Monad in which global Toil happens.
type GlobalToilM
     = ReaderT GlobalToilEnv (StateT GlobalToilState GlobalToilMBase)

-- | Run given action in some monad capable of getting stakeholders'
-- stakes and logging.
runGlobalToilMBase ::
       forall m a. (WithLogger m)
    => (StakeholderId -> m (Maybe Coin))
    -> GlobalToilMBase a
    -> m a
runGlobalToilMBase stakeGetter = launchNamedPureLog foldF'
  where
    foldF' :: forall x. F StakesLookupF x -> m x
    foldF' =
        foldF $ \case
            StakesLookupF sId f -> f <$> stakeGetter sId

-- | Run 'GlobalToilM' action in some monad capable of getting
-- stakeholders' stakes and logging.
runGlobalToilM ::
       forall m a. (WithLogger m)
    => GlobalToilEnv
    -> GlobalToilState
    -> (StakeholderId -> m (Maybe Coin))
    -> GlobalToilM a
    -> m (a, GlobalToilState)
runGlobalToilM env gts stakeGetter =
    runGlobalToilMBase stakeGetter . usingStateT gts . usingReaderT env

-- | Get stake of a given stakeholder.
getStake :: StakeholderId -> GlobalToilM (Maybe Coin)
getStake id =
    (<|>) <$> use (gtsStakesView . svStakes . at id) <*> baseLookup id
  where
    baseLookup :: StakeholderId -> GlobalToilM (Maybe Coin)
    baseLookup i =
        lift $ lift $ lift $ F $ \kPure kFree -> kFree (StakesLookupF i kPure)

-- | Get total stake of all stakeholders.
getTotalStake :: GlobalToilM Coin
getTotalStake =
    maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

-- | Set stake of a given stakeholder.
setStake :: StakeholderId -> Coin -> GlobalToilM ()
setStake id c = gtsStakesView . svStakes . at id .= Just c

-- | Set total stake of all stakeholders.
setTotalStake :: Coin -> GlobalToilM ()
setTotalStake c = gtsStakesView . svTotal .= Just c

-- | Extended version of 'GlobalToilM'. It allows to put extra data
-- into reader context and extra state. It's needed for explorer which
-- has more complicated transaction processing.
type ExtendedGlobalToilM extraEnv extraState =
    ReaderT (GlobalToilEnv, extraEnv) (
        StateT (GlobalToilState, extraState) (
            GlobalToilMBase
    ))

-- | Natural transformation from 'GlobalToilM to 'ExtendedGlobalToilM'.
extendGlobalToilM :: GlobalToilM ~> ExtendedGlobalToilM extraEnv extraState
extendGlobalToilM = zoom _1 . magnify _1

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Lift 'UtxoM' action to 'LocalToilM'.
utxoMToLocalToilM :: UtxoM ~> LocalToilM
utxoMToLocalToilM = zoom ltsUtxoModifier

-- | Lift 'UtxoM' action to 'GlobalToilM'.
utxoMToGlobalToilM :: UtxoM ~> GlobalToilM
utxoMToGlobalToilM = mapReaderT f . magnify gteUtxo
  where
    f :: State UtxoModifier
      ~> StateT GlobalToilState (NamedPureLogger (F StakesLookupF))
    f = state . runState . zoom gtsUtxoModifier
