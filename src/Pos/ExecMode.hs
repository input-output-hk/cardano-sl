{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.ExecMode
    ( ExecMode(..)
    , ExecModeM
    ) where

import           Universum

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Fix           (MonadFix)
import qualified Control.Monad.Reader        as Mtl
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.Coerce                 (coerce)
import qualified Ether
import           Ether.Internal              (HasLens (..))
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor' (..), Mockable (..), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId)
import           System.Wlog                 (CanLog)

import           Pos.Util.Util               (PowerLift (..))

type family ExecModeM mode :: * -> *

newtype ExecMode mode a = ExecMode { unExecMode :: ExecModeM mode a }

deriving instance Functor (ExecModeM mode) => Functor (ExecMode mode)
deriving instance Applicative (ExecModeM mode) => Applicative (ExecMode mode)
deriving instance Monad (ExecModeM mode) => Monad (ExecMode mode)
deriving instance MonadIO (ExecModeM mode) => MonadIO (ExecMode mode)
deriving instance (Monad b, MonadBase b (ExecModeM mode)) => MonadBase b (ExecMode mode)
deriving instance MonadThrow (ExecModeM mode) => MonadThrow (ExecMode mode)
deriving instance MonadCatch (ExecModeM mode) => MonadCatch (ExecMode mode)
deriving instance MonadMask (ExecModeM mode) => MonadMask (ExecMode mode)
deriving instance MonadFix (ExecModeM mode) => MonadFix (ExecMode mode)

instance MonadBaseControl b (ExecModeM mode) => MonadBaseControl b (ExecMode mode) where
    type StM (ExecMode mode) a = StM (ExecModeM mode) a
    liftBaseWith f = ExecMode $ liftBaseWith $ \q -> f (q . unExecMode)
    restoreM s = ExecMode $ restoreM s

type instance ThreadId (ExecMode mode) = ThreadId (ExecModeM mode)
type instance Promise (ExecMode mode) = Promise (ExecModeM mode)
type instance SharedAtomicT (ExecMode mode) = SharedAtomicT (ExecModeM mode)
type instance SharedExclusiveT (ExecMode mode) = SharedExclusiveT (ExecModeM mode)
type instance Gauge (ExecMode mode) = Gauge (ExecModeM mode)
type instance ChannelT (ExecMode mode) = ChannelT (ExecModeM mode)
type instance Distribution (ExecMode mode) = Distribution (ExecModeM mode)
type instance Counter (ExecMode mode) = Counter (ExecModeM mode)

instance
    ( Mockable d (ExecModeM mode)
    , MFunctor' d (ExecMode mode) (ExecModeM mode)
    ) => Mockable d (ExecMode mode)
  where
    liftMockable dmt = ExecMode $ liftMockable $ hoist' unExecMode dmt

instance
    ( Mtl.MonadReader payload (ExecModeM mode)
    , HasLens tag payload r
    ) => Ether.MonadReader tag r (ExecMode mode)
  where
    ask =
        (coerce :: ExecModeM mode r -> ExecMode mode r)
        (Mtl.asks (view (lensOf @tag @payload @r)))
    local f =
        (coerce :: forall a.
            (ExecModeM mode a -> ExecModeM mode a) ->
            (ExecMode mode a -> ExecMode mode a))
        (Mtl.local (over (lensOf @tag @payload @r) f))

deriving instance CanLog (ExecModeM mode) => CanLog (ExecMode mode)

instance PowerLift m (ExecModeM mode) => PowerLift m (ExecMode mode) where
    powerLift = ExecMode . powerLift
