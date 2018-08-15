-- | Possible errors with files which stores secret and public keys.

module Pos.Util.UserKeyError
       ( UserKeyType (..)
       , UserKeyError (..)
       , KeyError (..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception)

-- | 'PublicKey' is for external wallets, 'SecretKey' is for internal wallets.
data UserKeyType
    = Public
    | Secret
    deriving Show

data UserKeyError
    = NotWritable
    | AlreadyLocked
    | IncorrectLock
    deriving Show

data KeyError = KeyError UserKeyType UserKeyError
    deriving Show

instance Exception KeyError
