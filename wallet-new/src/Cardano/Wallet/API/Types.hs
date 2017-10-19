{-- Types shared between different API versions. --}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Cardano.Wallet.API.Types where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text       as T
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

data WalletVersion = WalletVersion {
      ver_version :: APIVersion
    , ver_gitRev  :: T.Text
    -- ^ The current git revision
    } deriving (Show, Eq)

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
    toJSON = String . T.pack . show

deriveToJSON defaultOptions { fieldLabelModifier = drop 4 } ''WalletVersion

instance Arbitrary WalletVersion where
    arbitrary = WalletVersion <$> arbitrary <*> pure "6f1131adca2f0bc6d24c9181cabd2b9e0704fd79"
