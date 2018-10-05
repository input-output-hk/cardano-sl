{-# LANGUAGE DataKinds #-}

module Cardano.Cluster.Util.Spec
    (
    -- * Miscellaneous
      prop_AlternativeDefault
    , prop_runAsync

    -- * List
    , prop_stripFilterPrefix
    , prop_sizedSubsequenceLength
    , prop_sizedSubsequenceInclusion

    -- * String
    , prop_sToKkToSRoundTrip
    , prop_kToSsToKRoundTrip
    , prop_splitRoundTrip

    -- * Internal
    , (=/=)
    , time
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (cancel)
import           Data.List (isSubsequenceOf)
import           Data.Time (diffUTCTime, getCurrentTime)
import           Test.QuickCheck (Positive (..), Property, classify, conjoin,
                     counterexample, ioProperty, (.&&.), (===))

import           Cardano.Cluster.Util
import           Cardano.Cluster.Util.Arbitrary (PrefixedEnv (..),
                     SeparatedBy (..), SmallList (..), UpperCased (..))


-- * Miscellaneous

-- | Resulting value of (f |> a) is never empty
prop_AlternativeDefault
    :: (Alternative f, Eq (f Int), Show (f Int))
    => (f Int, Int)
    -> Property
prop_AlternativeDefault (base, def) = classify (base == empty) "empty" $
    if base == empty then
        (base |> def) === pure def
    else
        (base |> def) =/= empty

prop_runAsync
    :: Int
    -> Property
prop_runAsync n = ioProperty $ do
    ((handle, res), t) <- time (runAsync io)
    cancel handle
    return $ classify (t < 100) "< 100μs" $ res === n .&&. t < (10 * oneSecond)
  where
    io :: (Int -> IO ()) -> IO ()
    io yield = do
        yield n
        threadDelay (10 * oneSecond)


prop_stripFilterPrefix
    :: PrefixedEnv "TEST_" Int
    -> Property
prop_stripFilterPrefix (PrefixedEnv env) = conjoin
    [ conjoin (prop_noPrefix <$> result)
    , conjoin (prop_inclusion <$> result)
    ]
  where
    prefix           = "TEST_"
    result           = stripFilterPrefix prefix env
    prop_noPrefix    = not . isPrefixOf prefix . fst
    prop_inclusion   = (`elem` env) . first (prefix <>)

prop_sizedSubsequenceLength
    :: (Positive Int, SmallList Int)
    -> Property
prop_sizedSubsequenceLength (Positive i, SmallList xs) = classify (xs == empty) "empty" $
    conjoin $ map ((=== i) . length) (sizedSubsequences i xs)

prop_sizedSubsequenceInclusion
    :: (Positive Int, SmallList Int)
    -> Property
prop_sizedSubsequenceInclusion (Positive i, SmallList xs) = classify (xs == empty) "empty" $
    conjoin $ map (`isSubsequenceOf` xs) (sizedSubsequences i xs)


-- * String

prop_sToKkToSRoundTrip
    :: UpperCased (SeparatedBy "_")
    -> Property
prop_sToKkToSRoundTrip (UpperCased str) = classify (str == empty) "empty" $
    (kToS . sToK) str === str

prop_kToSsToKRoundTrip
    :: SeparatedBy "-"
    -> Property
prop_kToSsToKRoundTrip (SeparatedBy str) = classify (str == empty) "empty" $
    (sToK . kToS) str === str

prop_splitRoundTrip
    :: SeparatedBy "-"
    -> Property
prop_splitRoundTrip (SeparatedBy str) = classify (str == empty) "empty" $
    intercalate "-" (split '-' str) === str


--
-- Internal
--

-- | Like '/=', but prints a counterexample when it fails.
-- Source: QuickCheck@2.12 Test.QuickCheck.Property#(=/=)
infix 4 =/=
(=/=) :: (Eq a, Show a) => a -> a -> Property
x =/= y =
  counterexample (show x ++ interpret res ++ show y) res
  where
    res = x /= y
    interpret True  = " /= "
    interpret False = " == "


-- | Approximately measure time taken by an IO action, in microseconds
time :: IO a -> IO (a, Int)
time action = do
    start <- getCurrentTime
    a <- action
    end <- getCurrentTime
    let t = fromIntegral $ numerator $ toRational $ end `diffUTCTime` start
    return (a, t)
