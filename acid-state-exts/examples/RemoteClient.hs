module Main (main) where

import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Remote
import           Network
import           RemoteCommon
import           System.Environment
import           System.IO

------------------------------------------------------
-- This is how AcidState is used:

open :: IO (AcidState StressState)
open = openRemoteState skipAuthenticationPerform "localhost" (PortNumber 8080)

main :: IO ()
main = do args <- getArgs
          case args of
            ["checkpoint"]
              -> do acid <- open
                    createCheckpoint acid
            ["query"]
              -> do acid <- open
                    n <- query acid QueryState
                    putStrLn $ "State value: " ++ show n
            ["poke"]
              -> do acid <- open
                    putStr "Issuing 100k transactions... "
                    hFlush stdout
                    replicateM_ (100000-1) (scheduleUpdate acid PokeState)
                    update acid PokeState
                    putStrLn "Done"
            ["clear"]
              -> do acid <- open
                    update acid ClearState
                    createCheckpoint acid
            _ -> do putStrLn "Commands:"
                    putStrLn "  query            Prints out the current state."
                    putStrLn "  poke             Spawn 100k transactions."
                    putStrLn "  checkpoint       Create a new checkpoint."
