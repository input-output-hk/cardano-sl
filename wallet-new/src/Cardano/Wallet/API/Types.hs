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

--
-- Types
--

-- | An empty type which can be used to inject the Swagger summary (i.e. a short, 120 chars
-- description of what an endpoint should do) at the type level, directly in the Servant API.
data Summary (sym :: Symbol)

-- | An empty type which can be used to inject Swagger tags at the type level,
-- directly in the Servant API.
data Tags (tags :: [Symbol])

-- | The API version. V0 represents the "legacy" API, whereas V1 is the API version currently
-- in use.
data APIVersion = V0
                | V1
                deriving (Eq, Enum, Bounded)

--
-- Instances
--

-- | Instance of `HasServer` which erases a `Summary` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (Summary desc :> subApi) context where
  type ServerT (Summary desc :> subApi) m = ServerT subApi m
  route _ = route (Proxy @ subApi)

-- | Instance of `HasServer` which erases the `Tags` from its routing,
-- as the latter is needed only for Swagger.
instance (HasServer subApi context) => HasServer (Tags tags :> subApi) context where
  type ServerT (Tags tags :> subApi) m = ServerT subApi m
  route _ = route (Proxy @ subApi)

instance Arbitrary APIVersion where
  arbitrary = elements [minBound .. maxBound]

instance Show APIVersion where
  show V0 = "v0"
  show V1 = "v1"

instance ToJSON APIVersion where
  toJSON x = object ["version" .= show x]
