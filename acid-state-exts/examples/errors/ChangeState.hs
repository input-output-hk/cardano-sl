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

import qualified Data.Text           as Text

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data FirstState = FirstState String
    deriving (Show)

data SecondState = SecondState Text.Text
    deriving (Show)

$(deriveSafeCopy 0 'base ''FirstState)
$(deriveSafeCopy 0 'base ''SecondState)

------------------------------------------------------
-- The transaction we will execute over the state.

$(makeAcidic ''FirstState [])
$(makeAcidic ''SecondState [])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do putStrLn "This example simulates what happens when you modify your state type"
          putStrLn "without telling AcidState how to migrate from the old version to the new."
          putStrLn "Hopefully this program will fail with a readable error message."
          putStrLn ""
          firstAcid <- openLocalStateFrom "state/ChangeState" (FirstState "first state")
          createCheckpoint firstAcid
          closeAcidState firstAcid
          secondAcid <- openLocalStateFrom "state/ChangeState" (SecondState (Text.pack "This initial value shouldn't be used"))
          closeAcidState secondAcid
          putStrLn "If you see this message then something has gone wrong!"
