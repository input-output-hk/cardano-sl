{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Delegation.LedgerState
  ( LedgerState(..)
  -- * Genesis State
  , genesisId
  , genesisState
  -- * Validity Tests
  , valid
  -- * State Transitions
  , asStateTransition
  , delegatedStake
  , retirePools
  ) where

import           Crypto.Hash             (hash)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Char8   as BS
import           Data.List               (find)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (isJust, mapMaybe)
import           Data.Set                (Set)
import qualified Data.Set                as Set

import           Delegation.Certificates
import           Delegation.Keys
import           Delegation.StakePool
import           Delegation.Transaction
import           Delegation.UTxO
import           Delegation.Validation

-- |The representation of the state of a ledger after applying
-- the effects of a list of transactions. The method 'asStateTransition'
-- uses a transaction as a transformation on the ledger state.
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo        :: UTxO
    -- |The active accounts.
  , getAccounts    :: Map HashKey Coin
    -- |The active stake keys.
  , getStKeys      :: Set HashKey
    -- |The current delegations.
  , getDelegations :: Map HashKey HashKey
    -- |The active stake pools.
  , getStPools     :: Set HashKey
    -- |A map of retiring stake pools to the epoch when they retire.
  , getRetiring    :: Map HashKey Int
    -- |The current epoch.
  , getEpoch       :: Int
  } deriving (Show, Eq)

instance BA.ByteArrayAccess String where
  length        = BA.length . BS.pack
  withByteArray = BA.withByteArray . BS.pack

-- |The transaction Id for UTxO included at the begining of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash "in the begining"

-- |Creates the ledger state for an empty ledger which
-- contains the specified UTxO outputs.
genesisState :: [TxOut] -> LedgerState
genesisState outs = LedgerState
  (UTxO (Map.fromList
    [((TxIn genesisId idx), out) | (idx, out) <- zip [0..] outs]
  ))
  Map.empty
  Set.empty
  Map.empty
  Set.empty
  Map.empty
  0

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
witnessTxin :: Tx -> LedgerState -> Validity
witnessTxin (Tx txBody (Wits ws _)) l =
  if (Set.size ws) == (Set.size ins) && all (hasWitness ws) ins
    then Valid
    else Invalid [InsuffientTxWitnesses]
  where
    utxo = getUtxo l
    ins = inputs txBody
    hasWitness witnesses input =
      isJust $ find (isWitness txBody input utxo) witnesses
    isWitness tb inp unspent (WitTxin key sig) =
      verify key tb sig && authTxin key inp unspent

-- |Given a ledger state, determine if the certificate witnesses in a given
-- transaction are sufficient.
witnessCert :: Tx -> Validity
witnessCert (Tx txBody (Wits _ ws)) =
  if (Set.size ws) == (Set.size cs) && all (hasWitness ws) cs
    then Valid
    else Invalid [InsuffientTxWitnesses]
  where
    cs = certs txBody
    hasWitness witnesses cert = isJust $ find (isWitness txBody cert) witnesses
    isWitness txBdy cert (WitCert key sig) =
      verify key txBdy sig && authCert key cert
--TODO combine with witnessTxin?


-- |The maximum number of epochs in the future that a stake pool
-- can set for retirement.
maxEpochRetirement :: Int
maxEpochRetirement = 100 -- TODO based on k? find a realistic value

-- |Given a ledger state, determine if the given certificate is valid.
validCert :: LedgerState -> Cert -> Validity

validCert ls (RegKey key) =
  if Set.notMember (hashKey key) (getStPools ls)
  then Valid
  else Invalid [BadRegistration]
  -- TODO spec mentions signing the public stake key. needed?

validCert ls (DeRegKey key) =
  if Set.member (hashKey key) (getStKeys ls)
  then Valid
  else Invalid [BadDeregistration]
  -- TODO spec mentions signing the public stake key. needed?

validCert ls (Delegate (Delegation _ poolKey)) =
  if Set.member (hashKey poolKey) (getStPools ls)
  then Valid
  else Invalid [BadDelegation]

validCert _ (RegPool _) =
  if True -- What exactly is being signed?
  then Valid
  else Invalid [BadPoolRegistration]

validCert ls (RetirePool _ epoch) =
  if (getEpoch ls) < epoch && epoch < maxEpochRetirement
  then Valid
  else Invalid [BadPoolRetirement]
  -- TODO What exactly is being signed?

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs :: Tx -> LedgerState -> Validity
validInputs tx l =
  if (txins tx) `Set.isSubsetOf` unspentInputs (getUtxo l)
    then Valid
    else Invalid [BadInputs]
  where unspentInputs (UTxO utxo) = Map.keysSet utxo

-- |Determine if the balance of the ledger state would be effected
-- in an acceptible way by a transaction.
preserveBalance :: Tx -> LedgerState -> Validity
preserveBalance tx l =
  if balance (txouts tx) <= balance (txins tx <| (getUtxo l))
    then Valid
    else Invalid [IncreasedTotalBalance]

-- |Determine if the certificates in a given transaction are valid for
-- the given ledger state.
validCerts :: Tx -> LedgerState -> Validity
validCerts (Tx (TxBody _ _ cs) _) ls = foldMap (validCert ls) cs

-- |Determine if a given transaction is valid for a ledger state.
valid :: Tx -> LedgerState -> Validity
valid tx l =
  validInputs tx l
    <> preserveBalance tx l
    <> validCerts tx l
    <> witnessTxin tx l
    <> witnessCert tx

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Int -> LedgerState
retirePools ls epoch = ls
  { getStPools = Set.difference (getStPools ls) (Map.keysSet retiring)
  , getRetiring = active }
  where (active, retiring) = Map.partition ((/=) epoch) (getRetiring ls)

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: LedgerState -> Tx -> LedgerState
applyTxBody ls tx = ls { getUtxo = newUTxOs }
  where newUTxOs = (txins tx !<| (getUtxo ls) `union` txouts tx)

-- |Apply a certificate as a state transition function on the ledger state.
applyCert :: Cert -> LedgerState -> LedgerState
applyCert (RegKey key) ls = ls
  { getStKeys = (Set.insert (hashKey key) (getStKeys ls))
  , getAccounts = (Map.insert (hashKey key) (Coin 0) (getAccounts ls))}
applyCert (DeRegKey key) ls = ls
  { getStKeys = (Set.delete (hashKey key) (getStKeys ls))
  , getAccounts = Map.delete (hashKey key) (getAccounts ls)
  , getDelegations = Map.delete (hashKey key) (getDelegations ls)
    }
applyCert (Delegate (Delegation source target)) ls =
  ls {getDelegations =
    Map.insert (hashKey source) (hashKey target) (getDelegations ls)}
applyCert (RegPool sp) ls = ls
  { getStPools = (Set.insert hsk (getStPools ls))
  , getRetiring = Map.delete hsk (getRetiring ls)}
  where hsk = hashKey $ poolPubKey sp
applyCert (RetirePool key epoch) ls = ls {getRetiring = retiring}
  where retiring = Map.insert (hashKey key) epoch (getRetiring ls)

-- |Apply a collection of certificates as a state transition function on
-- the ledger state.
applyCerts :: LedgerState -> Set Cert -> LedgerState
applyCerts = Set.fold applyCert

-- |Apply a transaction as a state transition function on the ledger state.
applyTransaction :: LedgerState -> Tx -> LedgerState
applyTransaction ls tx = applyTxBody (applyCerts ls cs) tx
  where cs = (certs . body) tx

-- |Apply a transaction as a state transition function on the ledger state
-- in the case where the transaction is valid for the given ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition :: Tx -> LedgerState -> Either [ValidationError] LedgerState
asStateTransition tx ls =
  case valid tx ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyTransaction ls tx

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map HashKey Coin
delegatedStake ls = Map.fromListWith mappend delegatedOutputs
  where
    getOutputs (UTxO utxo) = Map.elems utxo
    addStake delegations (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup (HashKey hsk) delegations
      return (pool, c)
    addStake _ _ = Nothing
    outs = getOutputs . getUtxo $ ls
    delegatedOutputs = mapMaybe (addStake (getDelegations ls)) outs
