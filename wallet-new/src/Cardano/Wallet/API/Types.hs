{-- Types shared between different API versions. --}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Cardano.Wallet.API.Types where

import           Data.Aeson
import           GHC.TypeLits
import           Servant
import           Servant.API.Sub ((:>))
import           Test.QuickCheck

data Summary (sym :: Symbol)

-- | Instance of `HasServer` which erases a `Summary` from its routing,
-- as the latter is needed only for Swagger.
instance (KnownSymbol desc, HasServer subApi context)
      => HasServer (Summary desc :> subApi) context where

  type ServerT (Summary desc :> subApi) m = ServerT subApi m
  route _ = route (Proxy @ subApi)

data APIVersion = V0
                | V1
                deriving (Eq, Enum, Bounded)

instance Arbitrary APIVersion where
  arbitrary = elements [minBound .. maxBound]

instance Show APIVersion where
  show V0 = "v0"
  show V1 = "v1"

instance ToJSON APIVersion where
  toJSON x = object ["version" .= show x]
