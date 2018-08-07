{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base monad used in tests, some real-world functionality is emulated.
-- It's called 'Emulation' to follow the style used in node-sketch, where
-- real-world analogue is called 'Production'.

module Test.Pos.Block.Logic.Emulation
       ( ClockVar (..)
       , Emulation (..)
       , runEmulation
       , sudoLiftIO
       ) where

import           Universum

import           Control.Monad.Base (MonadBase (..))
import qualified Control.Monad.Trans.Control as MC
import qualified Crypto.Random as Rand
import           Data.Coerce (coerce)
import           Data.Time.Units (Microsecond)
import           System.Wlog (CanLog (..))
import           UnliftIO (MonadUnliftIO)

newtype ClockVar = ClockVar (IORef Microsecond)

newtype Emulation a = Emulation { unEmulation :: ReaderT ClockVar IO a }
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO)

instance Rand.MonadRandom Emulation where
    getRandomBytes = Emulation . lift . Rand.getRandomBytes

runEmulation :: Microsecond -> Emulation a -> IO a
runEmulation startTime m = do
    clockVar <- newIORef startTime
    runReaderT (unEmulation m) (ClockVar clockVar)

-- Lift IO without a warning.
sudoLiftIO :: IO a -> Emulation a
sudoLiftIO m = Emulation (liftIO m)

instance MonadIO Emulation where
    liftIO m = Emulation . liftIO $ do
        -- if you see a lot of stars in the test log, a hunting season for
        -- not-mocked operations is open.
        --
        -- It's currently commented because stars take too much buffer space
        -- (all of my tmux @volhovm)
        --
        -- putStr ("*" :: String)
        m

instance MonadBase IO Emulation where
    liftBase = liftIO

type LiftBaseWith b m a = (MC.RunInBase m b -> b a) -> m a

newtype LiftBaseWith' b m a = LBW { unLBW :: LiftBaseWith b m a }

coerceLiftBaseWith ::
    LiftBaseWith b (ReaderT ClockVar IO) a ->
    LiftBaseWith b Emulation             a
coerceLiftBaseWith lbw =
    unLBW (coerce (LBW lbw))

-- Bad instance! Bad! Kill it!
-- NB. the instance is correct, but
--    we don't need no IO actions
--    we don't need no flow control
--    no damn exceptions in the test logs
--    hey, -------, leave the code alone.
instance MC.MonadBaseControl IO Emulation where
    type StM Emulation a = a
    liftBaseWith = coerceLiftBaseWith MC.liftBaseWith
    restoreM =
      (coerce :: forall a .
        (a -> ReaderT ClockVar IO a) ->
        (a -> Emulation a))
      MC.restoreM

instance CanLog Emulation where
    dispatchMessage ln s t = Emulation $ dispatchMessage ln s t
