{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Util.TimeLimit
       ( execWithTimeLimit

       -- * TimeWarp helpers
       , CanLogInParallel
       , WaitingDelta (..)
       , logWarningLongAction
       , logWarningWaitOnce
       , logWarningWaitLinear
       , logWarningSWaitLinear
       , logWarningWaitInf
       , runWithRandomIntervals'
       , waitRandomInterval'
       , runWithRandomIntervals
       , runWithRandomIntervalsNow
       , waitRandomInterval
       ) where

import           Universum         hiding (bracket, finally)

import           Data.Time.Units   (Microsecond, Second, convertUnit)
import           Formatting        (sformat, shown, stext, (%))
import           Mockable          (Async, Async, Bracket, Delay, Mockable,
                                    delay, race, withAsync)
import           System.Wlog       (WithLogger, logWarning)

import           Pos.Crypto.Random (randomNumber)
import           Pos.Util.LogSafe  (logWarningS)

-- | Data type to represent waiting strategy for printing warnings
-- if action take too much time.
--
-- [LW-4]: this probably will be moved somewhere from here
data WaitingDelta
    = WaitOnce      Second              -- ^ wait s seconds and stop execution
    | WaitLinear    Second              -- ^ wait s, s * 2, s * 3  , s * 4  , ...      seconds
    | WaitGeometric Microsecond Double  -- ^ wait m, m * q, m * q^2, m * q^3, ... microseconds
    deriving (Show)

-- | Constraint for something that can be logged in parallel with other action.
type CanLogInParallel m =
    (Mockable Delay m, Mockable Async m, WithLogger m, MonadIO m)


-- | Run action and print warning if it takes more time than expected.
logWarningLongAction
    :: forall m a.
       CanLogInParallel m
    => Bool -> WaitingDelta -> Text -> m a -> m a
logWarningLongAction secure delta actionTag action =
    -- Previous implementation was
    --
    --   bracket (fork $ waitAndWarn delta) killThread (const action)
    --
    -- but this has a subtle problem: 'killThread' can be interrupted even
    -- when exceptions are masked, so it's possible that the forked thread is
    -- left running, polluting the logs with misinformation.
    --
    -- 'withAsync' is assumed to take care of this, and indeed it does for
    -- 'Production's implementation, which uses the definition from the async
    -- package: 'uninterruptibleCancel' is used to kill the thread.
    withAsync (waitAndWarn delta) (const action)
  where
    logFunc :: Text -> m ()
    logFunc = bool logWarning logWarningS secure
    printWarning t = logFunc $ sformat ("Action `"%stext%"` took more than "%shown)
                                       actionTag t

    -- [LW-4]: avoid code duplication somehow (during refactoring)
    waitAndWarn (WaitOnce      s  ) = delay s >> printWarning s
    waitAndWarn (WaitLinear    s  ) =
        let waitLoop acc = do
                delay s
                printWarning acc
                waitLoop (acc + s)
        in waitLoop s
    waitAndWarn (WaitGeometric s q) =
        let waitLoop acc t = do
                delay t
                let newAcc = acc + t
                let newT   = round $ fromIntegral t * q
                printWarning (convertUnit newAcc :: Second)
                waitLoop newAcc newT
        in waitLoop 0 s

{- Helper functions to avoid dealing with data type -}

-- | Specialization of 'logWarningLongAction' with 'WaitOnce'.
logWarningWaitOnce :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitOnce = logWarningLongAction False . WaitOnce

-- | Specialization of 'logWarningLongAction' with 'WaiLinear'.
logWarningWaitLinear :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitLinear = logWarningLongAction False . WaitLinear

-- | Secure version of 'logWarningWaitLinear'.
logWarningSWaitLinear :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningSWaitLinear = logWarningLongAction True . WaitLinear

-- | Specialization of 'logWarningLongAction' with 'WaitGeometric'
-- with parameter @1.3@. Accepts 'Second'.
logWarningWaitInf :: CanLogInParallel m => Second -> Text -> m a -> m a
logWarningWaitInf = logWarningLongAction False . (`WaitGeometric` 1.3) . convertUnit

execWithTimeLimit
    :: ( Mockable Async m
       , Mockable Delay m
       , Mockable Bracket m
       )
    => Microsecond -> m a -> m (Maybe a)
execWithTimeLimit timeout action = do
    res <- race (delay timeout) action
    return $ case res of
        Left () -> Nothing
        Right a -> Just a

-- | Wait random number of 'Microsecond'`s between min and max.
waitRandomInterval
    :: (MonadIO m, Mockable Delay m)
    => Microsecond -> Microsecond -> m ()
waitRandomInterval minT maxT = do
    interval <-
        (+ minT) . fromIntegral <$>
        liftIO (randomNumber $ fromIntegral $ maxT - minT)
    delay interval

-- | Wait random interval and then perform given action.
runWithRandomIntervals
    :: (MonadIO m, WithLogger m, Mockable Delay m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervals minT maxT action = do
  waitRandomInterval minT maxT
  action
  runWithRandomIntervals minT maxT action

-- | Like `runWithRandomIntervals`, but performs action immidiatelly
-- at first time.
runWithRandomIntervalsNow
    :: (MonadIO m, WithLogger m, Mockable Delay m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervalsNow minT maxT action = do
  action
  runWithRandomIntervals minT maxT action

-- TODO remove MonadIO in preference to some `Mockable Random`
-- | Wait random number of 'Microsecond'`s between min and max.
waitRandomInterval'
    :: (MonadIO m, Mockable Delay m)
    => Microsecond -> Microsecond -> m ()
waitRandomInterval' minT maxT = do
    interval <-
        (+ minT) . fromIntegral <$>
        liftIO (randomNumber $ fromIntegral $ maxT - minT)
    delay interval

-- | Wait random interval and then perform given action.
runWithRandomIntervals'
    :: (MonadIO m, Mockable Delay m)
    => Microsecond -> Microsecond -> m () -> m ()
runWithRandomIntervals' minT maxT action = do
  waitRandomInterval' minT maxT
  action
  runWithRandomIntervals' minT maxT action
