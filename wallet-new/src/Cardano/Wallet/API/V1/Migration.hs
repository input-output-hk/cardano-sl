{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Cardano.Wallet.API.V1.Migration where

import qualified Cardano.Wallet.API.V1.Types      as V1
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

-- | 'Migrate' encapsulates migration between types, when possible.
class Migrate from to where
    migrate :: from -> to

--
-- Instances
--

instance Migrate V0.CWallet V1.Wallet where
    migrate V0.CWallet{..} =
        V1.Wallet { walId   = migrate @V0.WalletId
                  , walName = _
                  , walBalance = migrate @V0.CCoin @V1.RenderedBalance cwAmount
                  }

instance Migrate V0.CWalletAssurance V1.AssuranceLevel where
    migrate V0.CWAStrict = V1.StrictAssurance
    migrate V0.CWANormal = V1.NormalAssurance

instance Migrate V0.CWalletAssurance V1.AssuranceLevel where
    migrate V0.CWAStrict = V1.StrictAssurance
    migrate V0.CWANormal = V1.NormalAssurance

instance Migrate V0.CPassPhrase V1.SpendingPassword where
  migrate V0.CPassPhrase <$> newwalSpendingPassword
      initMeta   = V0.CWalletMeta newwalName newwalAssuranceLevel 0
  let walletInt  = V0.CWalletInit initMeta newwalBackupPhrase
  migrate <$> V0.newWallet spendingPassword walletInit
