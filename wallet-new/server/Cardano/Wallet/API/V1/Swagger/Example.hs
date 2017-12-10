{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.API.V1.Swagger.Example where

import           Universum

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types

import           Test.QuickCheck (Arbitrary (..), Gen, listOf1)

class Arbitrary a => Example a where
    example :: Gen a
    example = arbitrary

-- | Any @Example@ instance that we want to ovveride must be matched on
-- the type level in this definition here.
type family (F a) :: Bool where
  F [a]                = 'True
  F (Maybe a)          = 'True
  F (WalletResponse a) = 'True
  F Account            = 'True
  F NewWallet          = 'True
  F NodeInfo           = 'True
  F Payment            = 'True
  F a                  = 'False

instance (Arbitrary a, F a ~ flag, Example' flag a) => Example a where
  example = example' (Proxy :: Proxy flag)

class Example' (flag :: Bool) a where
  example' :: Proxy flag -> Gen a

instance Example a => Example' 'True [a] where
  example' _ = listOf1 example

instance Example a => Example' 'True (Maybe a) where
  example' _ = Just <$> example

instance (Arbitrary a) => Example' 'False a where
  example' _ = arbitrary

instance Example a => Example' 'True (WalletResponse a) where
  example' _ = WalletResponse <$> example
                              <*> pure SuccessStatus
                              <*> example

-- Specific instances that we want to ovveride

instance Example' 'True Account where
  example' _ = Account <$> example
                       <*> example -- NOTE: this will produce non empty list
                       <*> example
                       <*> pure "My account"
                       <*> example

instance Example' 'True NewWallet where
  example' _ = NewWallet <$> example
                         <*> example -- Note: will produce `Just a`
                         <*> example
                         <*> pure "My Wallet"

instance Example' 'True NodeInfo where
  example' _ = NodeInfo <$> example
                        <*> example  -- NOTE: will produce `Just a`
                        <*> example
                        <*> example

instance Example' 'True Payment where
  example' _ = Payment <$> example
                       <*> example
                       <*> example
                       <*> example -- TODO: will produce `Just groupingPolicy`

-- IMPORTANT: if executing `grep "[]\|null" wallet-new/spec/swagger.json` returns any element - then we have to add Example instances for those objects because we don't want to see [] or null examples in our docs.
--
-- TODO: We should probably add this as a part of our swagger CI script and fail swagger if we find some of them - with instruction to the developer above what is said above.
--
-- Most of it comes to removing Nothing from `Arbitrary (Maybe a)` instance and removing empty list from `Arbitrary [a]` instance. It could be done automatically with some quickcheck hacks but I think it would be an overkill.
