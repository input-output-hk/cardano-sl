module Delegation.Certificates
  (
    Cert(..)
  , authCert
  ) where

import           Delegation.Keys
import           Delegation.StakePool

-- | A heavyweight certificate.
data Cert = -- | A stake key registration certificate.
            RegKey VKey
            -- | A stake key deregistration certificate.
          | DeRegKey VKey --TODO this is actually HashKey on page 13, is that what we want?
            -- | A stake pool registration certificate.
          | RegPool StakePool
            -- | A stake pool retirement certificate.
          | RetirePool VKey Int
            -- | A stake delegation certificate.
          | Delegate Delegation
  deriving (Show, Eq, Ord)

-- |Determine if a certificate is authorized by the given key.
authCert :: VKey -> Cert -> Bool
authCert key cert = getRequiredSigningKey cert == key
  where
    getRequiredSigningKey (RegKey k)            = k
    getRequiredSigningKey (DeRegKey k)          = k
    getRequiredSigningKey (RegPool pool)        = poolPubKey pool
    getRequiredSigningKey (RetirePool k _)      = k
    getRequiredSigningKey (Delegate delegation) = delegator delegation
