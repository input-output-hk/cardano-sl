{-|
Module      : Delegation
Description : An Executable Model of the Delegation Design
Stability   : experimental

This is a stand-alone executable model for the delegation design.
The Delegation specification is
<https://github.com/input-output-hk/cardano-sl/blob/philipp/cdec-147/docs/delegation_design_spec.tex here>.
-}

module Delegation
  (
    Ledger
  , LedgerState(..)
  , Owner(..)
  , Addr(..)
  , VKey(..)
  , SKey(..)
  , KeyPair(..)
  , keyPair
  , HashKey
  , hashKey
  , sign
  , genesisId
  , genesisState
  , TxOut(..)
  , TxIn(..)
  , Coin(..)
  , UTxO(..)
  , Delegation(..)
  , Sig
  , StakePool(..)
  , TxBody(..)
  , Tx(..)
  , TxId(..)
  , Cert(..)
  , ValidationError(..)
  , Wits(..)
  , WitTxin(..)
  , WitCert(..)
  , asStateTransition
  , txid
  , balance
  , delegatedStake
  , retirePools
  ) where

import Delegation.Certificates
import Delegation.Keys
import Delegation.LedgerState
import Delegation.StakePool
import Delegation.Transaction
import Delegation.UTxO
import Delegation.Validation

-- |A ledger is a list of transactions.
type Ledger = [Tx]
