-- | Definition of a modular subscriber action, which maintains subscriptions
-- through failures etc.

module Pos.Infra.Diffusion.Subscription.Subscriber
    ( SubscriptionTarget (..)
    , constantSubscriptionTarget
    , listSubscriptionTarget
    , SubscribeTo
    , subscriber
    ) where

import           Universum

-- | Generate subscription targets in some monad.
-- TBD any value in using a streaming solution like conduit?
newtype SubscriptionTarget m target = SubscriptionTarget
    { getSubscriptionTarget :: m (Maybe (target, SubscriptionTarget m target))
    }

constantSubscriptionTarget :: Applicative m => target -> SubscriptionTarget m target
constantSubscriptionTarget target =
    SubscriptionTarget (pure (Just (target, constantSubscriptionTarget target)))

listSubscriptionTarget
    :: Applicative m
    => [target]
    -> SubscriptionTarget m target
listSubscriptionTarget []     = SubscriptionTarget $ pure Nothing
listSubscriptionTarget (t:ts) = SubscriptionTarget $ pure (Just (t, listSubscriptionTarget ts))

type SubscribeTo m target = target -> m ()

-- | Use a 'SubscriptionTarget m target' to generate 'target's to subscribe to.
-- A subscription is attempted to the generated target, and if/when it fails,
-- the next peer is generated and subscribed to, etc.
--
-- Be aware that, since this term is defined for any monad, it cannot do any
-- exception handling. If you choose an IO-capable monad, or once which can
-- otherwise do exception-like things, note that an unhandled exception in
-- the 'SubscriptionTarget' or 'SubscribeTo' will stop the whole thing.
subscriber
    :: ( Monad m )
    => SubscribeTo m target        -- ^ Do subscription.
    -> SubscriptionTarget m target -- ^ Generate targets.
    -> m ()
subscriber subscribeTo mkTarget = do
    mNext <- getSubscriptionTarget mkTarget
    case mNext of
        Nothing -> pure ()
        Just (target, mkTarget') -> do
            subscribeTo target
            subscriber subscribeTo mkTarget'
