{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( WalletMode
       ) where

import           Pos.Context           (WithNodeContext)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB         as Modern
#endif
import           Pos.Ssc.Class.Storage (SscStorageMode)
import           Pos.State             (MonadDB)
import           Pos.Txp.LocalData     (MonadTxLD)
import           Pos.WorkMode          (MinWorkMode)

type WalletMode ssc m
    = ( MinWorkMode m
      , MonadDB ssc m
#ifdef WITH_ROCKS
      , Modern.MonadDB ssc m
#endif
      , MonadTxLD m
      , WithNodeContext ssc m
      , SscStorageMode ssc
      )
