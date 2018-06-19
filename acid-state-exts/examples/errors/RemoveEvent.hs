{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid

import           Control.Monad.State
import           Data.SafeCopy
import           System.Environment

import           Data.Typeable

import           Control.Exception
import           Prelude             hiding (catch)

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data FirstState = FirstState
    deriving (Show)

data SecondState = SecondState
    deriving (Show)

$(deriveSafeCopy 0 'base ''FirstState)
$(deriveSafeCopy 0 'base ''SecondState)

------------------------------------------------------
-- The transaction we will execute over the state.

firstEvent :: Update FirstState ()
firstEvent = return ()

$(makeAcidic ''FirstState ['firstEvent])
$(makeAcidic ''SecondState [])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do putStrLn "This example simulates what happens when you remove an event"
          putStrLn "that is required to replay the journal."
          putStrLn "Hopefully this program will fail with a readable error message."
          putStrLn ""
          firstAcid <- openLocalStateFrom "state/RemoveEvent" FirstState
          update firstAcid FirstEvent
          closeAcidState firstAcid

          secondAcid <- openLocalStateFrom "state/RemoveEvent" SecondState
          closeAcidState secondAcid
          putStrLn "If you see this message then something has gone wrong!"
