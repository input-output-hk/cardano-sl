module Test.Integration.Framework.Scenario
    ( Scenario
    ) where

import           Universum

import           Test.Hspec.Core.Spec (Example (..), Result (..),
                     ResultStatus (..))


-- | A Wrapper around 'StateT' around which we define a few instances. The most
-- interesting one is 'Example'
newtype Scenario s m a = Scenario (StateT s m a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadFail
        , MonadIO
        , MonadMask
        , MonadState s
        )

-- | We emulate the 'MonadReader' using the 'MonadState' instance, this way, we
-- can lower down our constraints for methods that actually just read the state.
instance (Monad m, MonadState s (Scenario s m)) => MonadReader s (Scenario s m) where
    ask = get
    local f m = do
        s <- get
        put (f s) *> m <* put s

-- | This gives us the ability to define our spec as `Scenario` instead of just
-- plain `IO`. This way, each scenario runs within a context and has access to
-- a wallet client and a dedicated faucet wallet.
instance Example (Scenario s IO ()) where
    type Arg (Scenario s IO ()) = MVar s
    evaluateExample (Scenario io) _ action _ =
        action runAndPersist >> return (Result "" Success)
      where
        runAndPersist :: MVar s -> IO ()
        runAndPersist mvar = do
            let acquire = takeMVar mvar
            let release = putMVar mvar
            let between = runStateT io >=> (putMVar mvar . snd)
            bracketOnError acquire release between
