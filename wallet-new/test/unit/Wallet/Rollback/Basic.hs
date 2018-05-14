{-# LANGUAGE TemplateHaskell #-}

-- | Specification of rollback
module Wallet.Rollback.Basic (
    -- * State
    State(..)
  , stateCurrent
  , stateCheckpoints
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

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data State h a = State {
      _stateCurrent     :: Basic.State h a
    , _stateCheckpoints :: [Basic.State h a]
    }

makeLenses ''State

initState :: State h a
initState = State {
      _stateCurrent     = Basic.initState
    , _stateCheckpoints = []
    }

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

mkWallet :: (Hash h a, Ord a, Buildable st)
         => Ours a -> Lens' st (State h a) -> WalletConstr h a st
mkWallet ours l self st = (Basic.mkWallet ours (l . stateCurrent) self st) {
      applyBlock = \b -> self (st & l %~ applyBlock' ours b)
    , rollback   = self (st & l %~ rollback')
    , change     = utxoRemoveInputs (txIns (pending this))
                 $ utxoRestrictToOurs ours
                 $ txOuts (pending this)
    }
  where
    this = self st

walletEmpty :: (Hash h a, Ord a, Buildable a) => Ours a -> Wallet h a
walletEmpty ours = fix (mkWallet ours identity) initState

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

applyBlock' :: Hash h a
            => Ours a -> Block h a -> State h a -> State h a
applyBlock' ours b State{..} = State{
      _stateCurrent     = Basic.applyBlock' ours b _stateCurrent
    , _stateCheckpoints = _stateCurrent : _stateCheckpoints
    }

rollback' :: (Hash h a, Ord a) => State h a -> State h a
rollback' State{ _stateCheckpoints = [] } = error "rollback': no checkpoints"
rollback' State{ _stateCheckpoints = prev : checkpoints'
               , _stateCurrent     = curr
               } = State{
      _stateCurrent = Basic.State{
          _stateUtxo    = prev ^. Basic.stateUtxo
        , _statePending = (curr ^. Basic.statePending) `Set.union`
                          (prev ^. Basic.statePending)
        }
    , _stateCheckpoints = checkpoints'
    }

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (State h a) where
  build State{..} = bprint
    ( "State"
    % "{ current:     " % build
    % ", checkpoints: " % listJson
    % "}"
    )
    _stateCurrent
    _stateCheckpoints
