module Delegation.Validation
  ( ValidationError(..)
  , Validity(..)
  ) where

-- Specific validation errors for transactions.
data ValidationError =
                     -- | The UTxO inputs in the transation are not valid.
                       BadInputs
                     -- | The transaction results in an increased total balance of the ledger.
                     | IncreasedTotalBalance
                     -- | The transaction does not have all the transaction witnesses required.
                     | InsuffientTxWitnesses
                     -- | The transaction does not have all the certificate witnesses required.
                     | InsuffientCertWitnesses
                     -- | The transaction contains an invalid stake registration certificate.
                     | BadRegistration
                     -- | The transaction contains an invalid stake deregistration certificate.
                     | BadDeregistration
                     -- | The transaction contains an invalid stake delegation certificate.
                     | BadDelegation
                     -- | The transaction contains an invalid pool registration certificate.
                     | BadPoolRegistration
                     -- | The transaction contains an invalid pool retirement certificate.
                     | BadPoolRetirement
                     deriving (Show, Eq)

-- |The validity of a transaction, where an invalid transaction
-- is represented by list of errors.
data Validity = Valid | Invalid [ValidationError] deriving (Show, Eq)

instance Semigroup Validity where
  Valid <> b                 = b
  a <> Valid                 = a
  (Invalid a) <> (Invalid b) = Invalid (a ++ b)

instance Monoid Validity where
  mempty = Valid
  mappend = (<>)
