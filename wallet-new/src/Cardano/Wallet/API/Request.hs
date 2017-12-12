{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Cardano.Wallet.API.Request (
    RequestParams (..)
  -- * Handly re-exports
  , module Cardano.Wallet.API.Request.Pagination
  , module Cardano.Wallet.API.Request.Filter
  ) where

import           Cardano.Wallet.API.Request.Filter (FilterBy, FilterOperation)
import           Cardano.Wallet.API.Request.Pagination (PaginationMetadata (..), PaginationParams)

data RequestParams = RequestParams
    { rpPaginationParams :: PaginationParams
    -- ^ The pagination-related parameters
    }
