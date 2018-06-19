{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid
import           Data.Acid.Remote

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy
import           Network
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Typeable

import qualified Data.Map             as Map

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = String
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          acid <- openLocalState (KeyValue Map.empty)
          case args of
            [key]
              -> do mbKey <- query acid (LookupKey key)
                    case mbKey of
                      Nothing    -> putStrLn $ key ++ " has no associated value."
                      Just value -> putStrLn $ key ++ " = " ++ value
            [key,val]
              -> do update acid (InsertKey key val)
                    putStrLn "Done."
            _ -> do putStrLn "Usage:"
                    putStrLn "  key               Lookup the value of 'key'."
                    putStrLn "  key value         Set the value of 'key' to 'value'."
          closeAcidState acid
