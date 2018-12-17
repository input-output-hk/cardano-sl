{-# LANGUAGE BangPatterns #-}

module Test.Pos.Infra.Diffusion.Subscription.StatusSpec
    ( spec
    ) where

import           Prelude

import           Data.List ((\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Hspec (SpecWith, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, Property, elements, forAllShrink,
                     ioProperty, shrinkList, sized, (===))

import           Pos.Infra.Diffusion.Subscription.Status (Changes,
                     SubscriptionStates, SubscriptionStatus (..), changes,
                     emptySubscriptionStates, subscribed, subscribing,
                     terminated, withChanges)

data Key = A | B | C | D
  deriving (Eq, Ord, Show)

newtype Inputs = Inputs
    { getInputs :: [(Key, Maybe SubscriptionStatus)]
    }
    deriving (Show)

newtype Expectations = Expectations [Map Key SubscriptionStatus]
    deriving (Eq, Show)

type Observations = Expectations

-- | Produce the expected observations from inputs. It will have the same
-- length as the input list.
expectations :: Inputs -> Expectations
expectations = Expectations . expectations_ Map.empty . getInputs
  where
    expectations_
        :: Map Key SubscriptionStatus
        -> [(Key, Maybe SubscriptionStatus)]
        -> [Map Key SubscriptionStatus]
    expectations_ current inputList = case inputList of
        [] -> []
        (key, val) : inputList' ->
            let !expected = Map.alter (const val) key current
            in  expected : expectations_ expected inputList'

genKey :: Gen Key
genKey = elements [A, B, C, D]

-- | Ensure that every input induces a change in the subscription state.
-- That's only to say, for any (k :: Key), if it appears in the list paired
-- with (v :: Maybe SubscriptionStatus), then the next occurrence of k (if
-- any) is with v' /= v.
genInputs :: Gen Inputs
genInputs = Inputs <$> sized (genInputs_ Map.empty [])
  where
    genInputs_
        :: Map Key SubscriptionStatus
        -> [(Key, Maybe SubscriptionStatus)]
        -> Int
        -> Gen [(Key, Maybe SubscriptionStatus)]
    genInputs_ m acc n
        | n <= 0 = pure (reverse acc)
        | otherwise = do
              key <- genKey
              let current = Map.lookup key m
              val <- elements (subscriptionStatuses \\ [current])
              genInputs_ (Map.alter (const val) key m) ((key, val) : acc) (n - 1)
    subscriptionStatuses = [Nothing, Just Subscribing, Just Subscribed]

shrinkInputs :: Inputs -> [Inputs]
shrinkInputs (Inputs list) = Inputs <$> shrinkList (const []) list

property :: Property
property = forAllShrink genInputs shrinkInputs $ \inputs -> ioProperty $ do
    observed <- getObservations inputs
    pure $ expectations inputs === observed

getObservations :: Inputs -> IO Observations
getObservations (Inputs inputs) = do
    subscriptionStates <- emptySubscriptionStates
    start inputs subscriptionStates (changes subscriptionStates)
  where
    start
        :: [(Key, Maybe SubscriptionStatus)]
        -> SubscriptionStates Key
        -> Changes Key
        -> IO Observations
    start [] _ _ = pure (Expectations [])
    start (initial : rest) states changes' = do
        feedInput initial states
        (_, observed) <- withChanges changes' step (rest, states, [])
        pure (Expectations (reverse observed))

    step
        :: ([(Key, Maybe SubscriptionStatus)], SubscriptionStates Key, [Map Key SubscriptionStatus])
        -> Map Key SubscriptionStatus
        -> IO ( Either ([(Key, Maybe SubscriptionStatus)], SubscriptionStates Key, [Map Key SubscriptionStatus])
                       [Map Key SubscriptionStatus]
              )
    step ([], _, observed) m = pure (Right (m : observed))
    step ((input : rest), states, observed) m = do
        feedInput input states
        pure (Left (rest, states, m : observed))

    feedInput :: (Key, Maybe SubscriptionStatus) -> SubscriptionStates Key -> IO ()
    feedInput (key, val) states = case val of
        Nothing          -> terminated states key
        Just Subscribing -> subscribing states key
        Just Subscribed  -> subscribed states key

spec :: SpecWith ()
spec = describe "Status" $ prop "state change consistency" property
