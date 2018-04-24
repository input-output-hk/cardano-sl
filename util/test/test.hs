import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "\nDummy test program that fails to validate that a test failure \
            \causes a CI build to fail.\n"
    exitFailure
