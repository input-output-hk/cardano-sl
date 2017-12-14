{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Cardano.Wallet.API.Request (
    RequestParams (..)
  -- * Handly re-exports
  , module Cardano.Wallet.API.Request.Pagination
  , module Cardano.Wallet.API.Request.Filter
  , module Cardano.Wallet.API.Request.Sort
  ) where

import           Cardano.Wallet.API.Request.Filter
import           Cardano.Wallet.API.Request.Pagination (PaginationMetadata (..), PaginationParams)
import           Cardano.Wallet.API.Request.Sort

data RequestParams = RequestParams
    { rpPaginationParams :: PaginationParams
    -- ^ The pagination-related parameters
    }
