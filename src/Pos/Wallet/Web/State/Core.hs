-- | Keeps instance of `MonadGState`.

module Pos.Wallet.Web.State.Core () where

import           Universum

import qualified Pos.Constants               as Const
import           Pos.DB.Class                (MonadGState (..))
import           Pos.Wallet.Web.State.Holder (WalletWebDB)

-- TODO [CSL-803]: don't rely on constants
instance MonadIO m => MonadGState (WalletWebDB m) where
    gsAdoptedBVData = return Const.genesisBlockVersionData
