module Main where

import           Universum

import           Test.QuickCheck (Result (..), Testable, label,
                     quickCheckResult, withMaxSuccess)

import           Cardano.Cluster.Util.Spec (prop_AlternativeDefault,
                     prop_kToSsToKRoundTrip, prop_runAsync,
                     prop_sToKkToSRoundTrip, prop_sizedSubsequenceInclusion,
                     prop_sizedSubsequenceLength, prop_splitRoundTrip,
                     prop_stripFilterPrefix)

import           Cardano.Cluster.Environment.Spec
                     (prop_coresAndRelaysTopologyStatic,
                     prop_edgesConnectedToAllRelays,
                     prop_edgesTopologyBehindNat,
                     prop_generatedEnvironmentIsValid)


main :: IO ()
main = evalResults
    [
    -- Cardano.Cluster.Util
      prop "AlternativeDefault @Maybe" (prop_AlternativeDefault @Maybe)
    , prop "AlternativeDefault @[]" (prop_AlternativeDefault @[])
    , prop "runAsync" prop_runAsync
    , prop "stripFilterPrefix" prop_stripFilterPrefix
    , prop "sToKkToSRoundTrip" prop_sToKkToSRoundTrip
    , prop "kToSsToKRoundTrip" prop_kToSsToKRoundTrip
    , prop "splitRoundTrip" prop_splitRoundTrip
    , prop "sizedSubsequenceLength" prop_sizedSubsequenceLength
    , prop "sizedSubsequenceInclusion" prop_sizedSubsequenceInclusion

    -- Cardano.Cluster.Environment
    , prop "generatedEnvironmentIsValid" prop_generatedEnvironmentIsValid
    , prop "coresAndRelaysTopologyStatic" prop_coresAndRelaysTopologyStatic
    , prop "edgesTopologyBehindNat" prop_edgesTopologyBehindNat
    , prop "prop_edgesConnectedToAllRelays" prop_edgesConnectedToAllRelays
    ]


-- NOTE running 'quickCheck prop' doesn't make 'cabal test' fails
-- even if the property fails. So this little one cope with this
-- by running all specs and failing if one of them returned a failure.
evalResults :: [IO (String, Result)] -> IO ()
evalResults =
    sequence >=> (mapM_ $ \case
        (_, Success {}) -> return ()
        (p, _)          -> putTextLn (nice p) >> exitFailure)
  where
    nice p = toText ("Property failed: prop_" <> p)

prop :: Testable prop => String -> prop -> IO (String, Result)
prop lbl p =
    (lbl,) <$> quickCheckResult (withMaxSuccess 1000 $ label lbl p)
