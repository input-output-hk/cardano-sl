{-# LANGUAGE TemplateHaskell #-}

-- | Full version of the wallet (incremental, prefiltering, rollback)
module Wallet.Rollback.Full (
    -- * State
    Checkpoint(..)
  , checkpointIncr
  , checkpointExpected
  , checkpointUtxo
  , checkpointPending
  , checkpointUtxoBalance
  , State(..)
  , stateCurrent
  , stateCheckpoints
  , stateIncr
  , initState
    -- * Construction
  , mkWallet
  , walletEmpty
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util (listJson)

import           UTxO.DSL
import           Wallet.Abstract
import qualified Wallet.Basic as Basic
import qualified Wallet.Incremental as Incr

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data Checkpoint h a = Checkpoint {
      _checkpointIncr     :: Incr.State h a
    , _checkpointExpected :: Utxo h a
    }

data State h a = State {
      _stateCurrent     :: Checkpoint h a
    , _stateCheckpoints :: [Checkpoint h a]
    }

makeLenses ''Checkpoint
makeLenses ''State

checkpointUtxo :: Lens' (Checkpoint h a) (Utxo h a)
checkpointUtxo = checkpointIncr . Incr.stateUtxo

checkpointPending :: Lens' (Checkpoint h a) (Pending h a)
checkpointPending = checkpointIncr . Incr.statePending

checkpointUtxoBalance :: Lens' (Checkpoint h a) Value
checkpointUtxoBalance = checkpointIncr . Incr.stateUtxoBalance

stateIncr :: Lens' (State h a) (Incr.State h a)
stateIncr = stateCurrent . checkpointIncr

initState :: State h a
initState = State {
      _stateCurrent = Checkpoint {
           _checkpointIncr     = Incr.initState
         , _checkpointExpected = utxoEmpty
         }
    , _stateCheckpoints = []
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

mkWallet :: (Hash h a, Ord a, Buildable st)
         => Ours a -> Lens' st (State h a) -> WalletConstr h a st
mkWallet ours l self st = (Incr.mkWallet ours (l . stateIncr) self st) {
      applyBlock = \b ->
        let utxoPlus   = utxoRestrictToOurs ours (txOuts b)
            filterUtxo = utxoUnions [ utxo this
                                    , st ^. l . stateCurrent . checkpointExpected
                                    , utxoPlus
                                    ]
            filtered   = txIns b `Set.intersection` utxoDomain filterUtxo
        in self (st & l %~ applyBlock' (filtered, utxoPlus))
    , rollback = self (st & l %~ rollback')
    , change = utxoRemoveInputs (txIns (pending this))
             $ utxoRestrictToOurs ours
             $ txOuts (pending this)
    , expectedUtxo = st ^. l . stateCurrent . checkpointExpected
    }
  where
    this = self st

walletEmpty :: (Hash h a, Ord a, Buildable a) => Ours a -> Wallet h a
walletEmpty ours = fix (mkWallet ours identity) initState

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

applyBlock' :: Hash h a
            => (Set (Input h a), Utxo h a)
            -> State h a -> State h a
applyBlock' (ins, outs) State{..} = State{
      _stateCurrent = Checkpoint {
           _checkpointIncr     = Incr.applyBlock' (ins, outs) _checkpointIncr
         , _checkpointExpected = utxoRemoveInputs ins _checkpointExpected
         }
    , _stateCheckpoints = _stateCurrent : _stateCheckpoints
    }
  where
    Checkpoint{..} = _stateCurrent
    Incr.State{..} = _checkpointIncr

rollback' :: (Hash h a, Ord a) => State h a -> State h a
rollback' State{ _stateCheckpoints = [] } = error "rollback': no checkpoints"
rollback' State{ _stateCheckpoints = prev : checkpoints'
               , _stateCurrent     = curr
               } = State{
      _stateCurrent = Checkpoint {
          _checkpointIncr = Incr.State {
               _stateBasic = Basic.State {
                   _stateUtxo    = prev ^. checkpointUtxo
                 , _statePending = (curr ^. checkpointPending) `Set.union`
                                   (prev ^. checkpointPending)
                 }
             , _stateUtxoBalance = prev ^. checkpointUtxoBalance
             }
        , _checkpointExpected = utxoUnions [
              curr ^. checkpointExpected
            , prev ^. checkpointExpected
            , utxoRemoveInputs (utxoDomain (prev ^. checkpointUtxo))
                                           (curr ^. checkpointUtxo)
            ]
        }
    , _stateCheckpoints = checkpoints'
    }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (Checkpoint h a) where
  build Checkpoint{..} = bprint
    ( "Checkpoint"
    % "{ incr:     " % build
    % ", expected: " % build
    % "}"
    )
    _checkpointIncr
    _checkpointExpected

instance (Hash h a, Buildable a) => Buildable (State h a) where
  build State{..} = bprint
    ( "State"
    % "{ current:     " % build
    % ", checkpoints: " % listJson
    % "}"
    )
    _stateCurrent
    _stateCheckpoints
