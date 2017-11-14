{- | This is a temporary module to help migration @V0@ datatypes into @V1@ datatypes.
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Cardano.Wallet.API.V1.Migration (
      MonadV1
    , V1Context
    , v1MonadNat
    , lowerV1Monad
    , Migrate(..)
    -- * Isomorphisms
    , walletAssurance
    -- * Configuration re-exports
    , HasInfraConfiguration
    , HasSscConfiguration
    , HasCompileInfo
    , HasConfiguration
    ) where

import           Universum

import qualified Cardano.Wallet.API.V1.Types      as V1
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

import           Control.Lens
import qualified Control.Monad.Catch              as Catch
import           Mockable                         (runProduction)
import           Servant

import           Pos.Core.Configuration           (HasConfiguration)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Ssc                          (HasSscConfiguration)
import           Pos.Util.CompileInfo             (HasCompileInfo)
import           Pos.Wallet.Web.Mode              (WalletWebMode, WalletWebModeContext)

-- | Temporary monad to handle the migration from the V0 & V1 stacks.
type MonadV1   = WalletWebMode
type V1Context = WalletWebModeContext

-- | Hoist a 'V1' monad to a Servant's Handler.
-- See: http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#natural-transformations
v1MonadNat :: V1Context -> (MonadV1 :~> Handler)
v1MonadNat ctx = NT (lowerV1Monad ctx)

-- | Converts our domain-specific monad into a standard Servant `Handler`.
lowerV1Monad :: V1Context -> MonadV1 a -> Handler a
lowerV1Monad ctx handler =
    liftIO (hoistHandler handler) `Catch.catches` excHandlers
  where

    hoistHandler :: forall a . MonadV1 a -> IO a
    hoistHandler = runProduction . flip runReaderT ctx

    excHandlers = [Catch.Handler throwError]

-- | 'Migrate' encapsulates migration between types, when possible.
class Migrate from to where
    migrate :: from -> to

--
-- Instances
--

instance Migrate V0.CWallet V1.Wallet where
    migrate V0.CWallet{..} =
        V1.Wallet { V1.walId   = migrate @(V0.CId V0.Wal) cwId
                  , V1.walName = V0.cwName cwMeta
                  , V1.walBalance = migrate @V0.CCoin @V1.RenderedBalance cwAmount
                  }

--
instance Migrate V0.CWalletAssurance V1.AssuranceLevel where
    migrate V0.CWAStrict = V1.StrictAssurance
    migrate V0.CWANormal = V1.NormalAssurance

instance Migrate V1.AssuranceLevel V0.CWalletAssurance where
    migrate V1.StrictAssurance = V0.CWAStrict
    migrate V1.NormalAssurance = V0.CWANormal

walletAssurance :: Iso' V0.CWalletAssurance V1.AssuranceLevel
walletAssurance = iso migrate migrate

--
instance Migrate V0.CPassPhrase V1.SpendingPassword where
  migrate (V0.CPassPhrase pp) = pp

instance Migrate V1.SpendingPassword V0.CPassPhrase where
  migrate pp = V0.CPassPhrase pp

--
instance Migrate V0.CCoin V1.RenderedBalance where
    migrate (V0.CCoin c) = V1.RenderedBalance c

instance Migrate V1.RenderedBalance V0.CCoin where
    migrate (V1.RenderedBalance c) = V0.CCoin c

--
instance Migrate (V0.CId V0.Wal) V1.WalletId where
    migrate (V0.CId (V0.CHash h)) = V1.WalletId h
