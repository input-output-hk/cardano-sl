module Test.Pos.Infra.Diffusion.Subscription.SubscriptionSpec
    ( spec
    ) where

import           Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import           Control.Concurrent.Async (async, cancel, waitCatch)
import           Control.Exception (AsyncException (ThreadKilled), throwIO, fromException)
import           System.IO.Error (userError)
import           Test.Hspec (Expectation, Spec, describe, it, shouldBe, expectationFailure)

import           Pos.Diffusion.Subscription.Common (SubscriptionTerminationReason (..),
                                                    networkSubscribeTo)

spec :: Spec
spec = describe "Exception handling" $ do
    it "networkSubscribeTo squelches synchronous exceptions" syncExceptionSpec
    it "networkSubscribeTo does not squelch asynchronous exceptions" asyncExceptionSpec

syncExceptionSpec :: Expectation
syncExceptionSpec = do

    -- This one throws an IOException. We check that it fails normally by
    -- returning 'Exceptional' containing the exception.
    reason <- networkSubscribeTo
        (pure ())
        (\reason _ _ -> pure reason)
        (throwIO ioerror)

    case reason of
        Normal -> expectationFailure "expected exceptional termination reason"
        Exceptional someException -> fromException someException `shouldBe` Just ioerror
  where
    ioerror = userError "failure"

asyncExceptionSpec :: Expectation
asyncExceptionSpec = do

    -- This one hangs indefinitely but is killed by an asynchronous exception
    -- ('AsyncCancelled'). We check that it fails exceptionally: does *not*
    -- return an 'Exceptional'.
    mvar <- newEmptyMVar
    thread <- async $ networkSubscribeTo
        (pure ())
        (\reason _ _ -> pure reason)
        (takeMVar mvar)
    _ <- cancel thread
    reason <- waitCatch thread

    case reason of
        -- FIXME this will change to AsyncCancelled when we upgrade to
        -- async >=2.2 with the LTS-11.2 change.
        -- But who knows when that will finally pass CI????
        Left someException -> fromException someException `shouldBe` Just ThreadKilled
        Right Normal -> expectationFailure "thread finished normally"
        -- The thread should die exceptionally rather than finish up and
        -- return 'Exceptional'.
        Right (Exceptional someException) -> expectationFailure
            ("thread finished exceptionally with " ++ show someException)
