{-
This example is mostly just to test that this bug is fixed:

https://github.com/acid-state/acid-state/issues/73

At the end of a run, the checkpoint file should contain a single
checkpoint and the event file should be empty. The old checkpoints and
events should be in the Archive directory.

In the Acrhive directory, each checkpoint file should contain one
checkpoint, and each event file should contain 10 events.

If you comment out the 'createArchive' line below, then the checkpoint
files should contain 10 checkpoints each.

-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

-- import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable
import           System.Environment

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

newtype Counter = Counter { unCounter :: Integer }
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Counter)

incCounter :: Update Counter Integer
incCounter =
  do (Counter c) <- get
     let c' = succ c
     put (Counter c')
     return c'

$(makeAcidic ''Counter ['incCounter])


main :: IO ()
main =
  do acid <- openLocalState (Counter 0)
     replicateM_ 10 $ do is <- replicateM 10 (update acid IncCounter)
                         print is
                         createCheckpoint acid
                         createArchive acid
     closeAcidState acid
