-- | Definition of a modular subscriber action, which maintains subscriptions
-- through failures etc.

module Pos.Diffusion.Subscription.Subscriber
    ( SubscriptionTarget (..)
    , constantSubscriptionTarget
    , SubscribeTo
    , subscriber
    ) where

import           Universum

-- | Generate subscription targets in some monad.
-- TBD any value in using a streaming solution like pipes?
newtype SubscriptionTarget m target = SubscriptionTarget
    { getSubscriptionTarget :: m (target, SubscriptionTarget m target)
    }

constantSubscriptionTarget :: Applicative m => target -> SubscriptionTarget m target
constantSubscriptionTarget target =
    SubscriptionTarget (pure (target, constantSubscriptionTarget target))

{-
listSubscriptionTarget
    :: Applicative m
    => [target]
    -> SubscriptionTarget m target
    -> SubscriptionTarget m target
listSubscriptionTarget []     tail = tail
listSubscriptionTarget (t:ts) tail = SubscriptionTarget (pure t, listSubscriptionTarget ts tail)
-}

type SubscribeTo m target = target -> m ()

-- | Use a 'SubscriptionTarget' to generate 'NodeId's to subscribe to. A
-- subscription is attempted to the generated peer, and if/when it fails, the
-- next peer is generated and subscribed to, etc.
--
-- Be aware that, since this term is defined for any monad, it cannot do any
-- exception handling. If you choose an IO-capable monad, or once which can
-- otherwise do exception-like things, note that an unhandled exception in
-- the 'SubscriptionTarget' or 'SubscribeTo' will stop the whole thing.
subscriber
    :: ( Monad m )
    => SubscribeTo m target        -- ^ Do subscription.
    -> SubscriptionTarget m target -- ^ Generate targets.
    -> m x
subscriber subscribeTo mkTarget = do
    (target, mkTarget') <- getSubscriptionTarget mkTarget
    subscribeTo target
    subscriber subscribeTo mkTarget'
