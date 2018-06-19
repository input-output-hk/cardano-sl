{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid
import           Data.Acid.Advanced

import           Control.Applicative
import           Control.Monad.Reader
import qualified Control.Monad.State  as State
import           Data.SafeCopy
import           System.Environment
import           System.IO

import           Data.Typeable

import qualified Data.Map             as Map

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = String
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

instance SafeCopy KeyValue where
    putCopy (KeyValue state) = contain $ safePut state
    getCopy = contain $ liftM KeyValue safeGet

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- State.get
         State.put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do acid <- openLocalState (KeyValue Map.empty)
          args <- getArgs
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
                    putStrLn "  key          Lookup the value of 'key'."
                    putStrLn "  key value    Set the value of 'key' to 'value'."
          closeAcidState acid



------------------------------------------------------
-- The gritty details. These things may be done with
-- Template Haskell in the future.

data InsertKey = InsertKey Key Value
data LookupKey = LookupKey Key


deriving instance Typeable InsertKey
instance SafeCopy InsertKey where
    putCopy (InsertKey key value) = contain $ safePut key >> safePut value
    getCopy = contain $ InsertKey <$> safeGet <*> safeGet
instance Method InsertKey where
    type MethodResult InsertKey = ()
    type MethodState InsertKey = KeyValue
instance UpdateEvent InsertKey

deriving instance Typeable LookupKey
instance SafeCopy LookupKey where
    putCopy (LookupKey key) = contain $ safePut key
    getCopy = contain $ LookupKey <$> safeGet
instance Method LookupKey where
    type MethodResult LookupKey = Maybe Value
    type MethodState LookupKey = KeyValue
instance QueryEvent LookupKey

instance IsAcidic KeyValue where
    acidEvents = [ UpdateEvent (\(InsertKey key value) -> insertKey key value)
                 , QueryEvent (\(LookupKey key) -> lookupKey key)
                 ]
