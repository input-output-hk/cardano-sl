{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-

An execution mode is a generalization of a newtype around the base monad layer.
In order to have different implementations of features in different execution
modes, there are two options:

* create different transformers (redirection IdentityT layers)
* create different newtypes around the base monad

The redirection approach is modular, but it has a fatal flaw: the types grow
too big and the compiler drowns in instance specialization. The build-time
performance is unacceptable.

The newtypes around the base layer are syntactically small and instance
specialization is fast:

@
    newtype RealMode ssc = RealMode (ReaderT (RealModeContext ssc) Production)
    newtype WebMode = WebMode (ReaderT WebModeContext Production)
    newtype LightWalletMode = LightWalletMode (ReaderT LightWalletContext Production)
@

The issue with creating a newtype per mode is that we don't have
the expressive power to implement a class/type instance for all modes at
once. For example, one would need to derive 'Functor', 'MonadIO', 'MonadMask',
and other classes, for every newtype. Type families and ambiguous type variables
make the issue worse because deriving can't handle them, so 'MonadBaseControl'
and 'Ether.MonadReader' would need manually defined instances, and 'Mockable'-related
type families would need numerous instances as well.

What we want is to implement some class and type instances for all modes at
once. The 'ExecMode' newtype makes it possible.

Instead of

@
    newtype PatakMode ssc =
        PatakMode (ReaderT (PatakCtx ssc) Production)
@

we write

@
    data PATAK ssc
    type PatakMode ssc = ExecMode (PATAK ssc)
    type instance ExecModeM (PATAK ssc) = ReaderT (PatakCtx ssc) Production
@

This way we can talk about some @'ExecMode' mode@, i.e. abstract over the
@mode@ parameter.

-}
module Pos.ExecMode
    ( ExecMode(..)
    , ExecModeM
    , _ExecMode
    , module Pos.ExecMode.Context
    ) where

import           Universum

import           Control.Lens                (makePrisms)
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

-- Re-exports.
import           Pos.ExecMode.Context

-- | The inner workings of an execution mode.
-- Example: @type instance ExecModeM Buba = ReaderT BubaEnv IO@.
type family ExecModeM mode :: * -> *

-- | A newtype over an execution mode. The mode itself is a parameter and
-- can be abstracted over. Keep the @mode@ parameter small, without structure
-- (ideally it's a single type name), to avoid the issues that plagued nested
-- transformer stacks.
newtype ExecMode mode a = ExecMode { unExecMode :: ExecModeM mode a }

makePrisms ''ExecMode

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

-- | The 'Ether.MonadReader' instance for 'ExecMode' uses a lens to focus
-- on a part of the inner 'ReaderT'. We use the 'HasLens' class to find a lens
-- and 'Mtl.MonadReader' to find the 'ReaderT' transformer. Since 'ExecMode'
-- is always a base layer, we don't need to \"close the world\" with a
-- type-level list of environment parts.
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
