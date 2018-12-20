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


import           System.IO (hSetEncoding, stderr, stdout, utf8)


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


-- NOTE: running 'quickCheck prop' doesn't make 'cabal test' fail
-- even if the property fails. This function runs all the properties and
-- fails if one or more them returned a failure.
evalResults :: [IO (String, Result)] -> IO ()
evalResults xs = do

    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    ys <- sequence xs
    case filter (not . isSuccess . snd) ys of
        [] -> pure ()
        zs -> do
            putTextLn "\nThe following tests failed:"
            mapM_ (\p -> putTestFailure $ fst p) zs
            exitFailure
  where
    putTestFailure p =
        putTextLn $ "  prop_" <> toText p

    -- This is in QuickCheck 2.12, but not the version suggested by stack.
    isSuccess Success{} = True
    isSuccess _         = False

prop :: Testable prop => String -> prop -> IO (String, Result)
prop lbl p =
    (lbl,) <$> quickCheckResult (withMaxSuccess 1000 $ label lbl p)
