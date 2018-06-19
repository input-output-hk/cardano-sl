{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid
import           Data.Acid.Advanced

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy
import           System.Environment

import           Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data HelloWorldState = HelloWorldState String
    deriving (Show, Typeable)

instance SafeCopy HelloWorldState where
    putCopy (HelloWorldState state) = contain $ safePut state
    getCopy = contain $ liftM HelloWorldState safeGet

------------------------------------------------------
-- The transaction we will execute over the state.

writeState :: String -> Update HelloWorldState ()
writeState newValue
    = put (HelloWorldState newValue)

queryState :: Query HelloWorldState String
queryState = do HelloWorldState string <- ask
                return string


------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do acid <- openLocalState (HelloWorldState "Hello world")
          args <- getArgs
          if null args
             then do string <- query acid QueryState
                     putStrLn $ "The state is: " ++ string
             else do update acid (WriteState (unwords args))
                     putStrLn "The state has been modified!"


------------------------------------------------------
-- The gritty details. These things may be done with
-- Template Haskell in the future.

data WriteState = WriteState String
data QueryState = QueryState


deriving instance Typeable WriteState
instance SafeCopy WriteState where
    putCopy (WriteState st) = contain $ safePut st
    getCopy = contain $ liftM WriteState safeGet
instance Method WriteState where
    type MethodResult WriteState = ()
    type MethodState WriteState = HelloWorldState
instance UpdateEvent WriteState

deriving instance Typeable QueryState
instance SafeCopy QueryState where
    putCopy QueryState = contain $ return ()
    getCopy = contain $ return QueryState
instance Method QueryState where
    type MethodResult QueryState = String
    type MethodState QueryState = HelloWorldState
instance QueryEvent QueryState


instance IsAcidic HelloWorldState where
    acidEvents = [ UpdateEvent (\(WriteState newState) -> writeState newState)
                 , QueryEvent (\QueryState             -> queryState)
                 ]
