module Util.Safe
    ( runWithFile
    , runWithFiles
    ) where

import Pipes.Safe         (runSafeT)
import Pipes.Safe.Prelude (withFile)

import Universum                  hiding (withFile)

runWithFile :: (MonadIO m, MonadMask m) =>  FilePath -> IOMode -> (Handle -> m r) -> m r
runWithFile fp mode f = runSafeT $ withFile fp mode $ lift . f

runWithFiles :: (MonadIO m, MonadMask m) => [(a, FilePath)] -> IOMode -> ([(a, Handle)]-> m r) -> m r
runWithFiles []             _    f = f []
runWithFiles ((a, fp) : xs) mode f = runWithFile fp mode $ \h ->
    runWithFiles xs mode $ \ys -> f $ (a, h) : ys
