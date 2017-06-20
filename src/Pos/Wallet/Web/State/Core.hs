-- | `MonadGState` implementation for web wallet.

module Pos.Wallet.Web.State.Core (gsAdoptedBVDataWebWallet) where

import           Universum

import           Pos.Core.Types              (BlockVersionData)
import qualified Pos.Constants               as Const

-- TODO [CSL-803]: don't rely on constants
gsAdoptedBVDataWebWallet :: Monad m => m BlockVersionData
gsAdoptedBVDataWebWallet =
    return Const.genesisBlockVersionData
