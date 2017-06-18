{-# LANGUAGE TypeFamilies #-}

-- | Keeps instance of `MonadGState`.

module Pos.Wallet.Light.State.Core
       ( gsAdoptedBVDataWallet
       ) where

import           Universum

import qualified Pos.Constants as Const
import           Pos.Core      (BlockVersionData)

-- Currently 'WalletState' doesn't maintain block version data.
gsAdoptedBVDataWallet :: Monad m => m BlockVersionData
gsAdoptedBVDataWallet = pure Const.genesisBlockVersionData
