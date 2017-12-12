module Cardano.Wallet.API.Response.Filter.IxSet where

import           Cardano.Wallet.API.Request.Filter
import qualified Data.IxSet.Typed as Ix

applyFilter :: IsIndexOf a ix => Ix.IxSet ixs a -> FilterOperation ix a -> Ix.IxSet ixs a
applyFilter inputData _ = inputData
