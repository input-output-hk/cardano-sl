import           Universum

import           Test.Hspec
    (hspec)

import           Spec
    (spec)

import qualified Test.Pos.Binary.BiSerialize

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Binary.BiSerialize.tests
        ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
    result <- andM tests
    unless result
        exitFailure
