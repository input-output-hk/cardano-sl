{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Fault injection, strategically placed at the root of CSL dependency graph.

module Pos.Infra.InjectFail
       ( FInjects
       , parseFInjectsSpec
       , mkFInjects
       , FInject(..)
       , testFInject
       , setFInject
       , logFInject
       , testLogFInject
       ) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Options.Applicative

import           Pos.Util.Wlog (CanLog, HasLoggerName, logError,
                     modifyLoggerName)



data FInject
  = FInjIgnoreAPI                     -- ^ Return a hard-coded string for all registered Wallet API endpoints
  | FInjIgnoreShutdown                -- ^ Ignore the shutdown request
  | FInjApplyUpdateNoExit             -- ^ Don't exit after handling the 'update/apply' endpoint
  | FInjApplyUpdateWrongExitCode      -- ^ Exit with a wrong exit code as response to the 'update/apply' request
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)
instance FromJSON FInject
instance ToJSON   FInject

desc :: FInject -> String
desc = \case
    FInjIgnoreAPI                     ->  "Return a hard-coded string for all registered Wallet API endpoints"
    FInjIgnoreShutdown                ->  "Ignore the shutdown request"
    FInjApplyUpdateNoExit             ->  "Don't exit after handling the 'update/apply' endpoint"
    FInjApplyUpdateWrongExitCode      ->  "Exit with a wrong exit code as response to the 'update/apply' request"

newtype FInjects = FInjects { _fromFInjs :: IORef (Set FInject) }

parseFInjectsSpec :: Parser (Set FInject)
parseFInjectsSpec = Set.fromList . catMaybes <$> sequenceA (parseSingle <$> enumFromTo minBound maxBound)

parseSingle :: FInject -> Parser (Maybe FInject)
parseSingle fi = (<&> \case
                     Just True -> Just fi
                     _         -> Nothing) <$> optional $
  switch (long (drop 4 $ map toLower $ show fi) <>
          help (desc fi))

-- | Make a stateful fault injection configuration object.
mkFInjects :: MonadIO m => Set FInject -> m FInjects
mkFInjects fs = liftIO $ FInjects <$> newIORef fs

-- | Test if code holding a reference, wants a particular fault injection enabled.
testFInject :: MonadIO m => FInjects -> FInject -> m Bool
testFInject (FInjects fsRef) fi =
  Set.member fi <$> readIORef fsRef
{-# INLINE testFInject #-}

-- | Signal enablement of particular fault injection to listeners.
setFInject :: MonadIO m => FInjects -> FInject -> Bool -> m ()
setFInject  (FInjects fsRef) fi enable =
  modifyIORef' fsRef (if enable then Set.insert fi else Set.delete fi)

logFInject :: (CanLog m, HasCallStack, HasLoggerName m)
           => FInject -> m ()
logFInject = modifyLoggerName (const "InjectFail") .
  logError . T.pack . (<> prettyCallStack callStack) . ("injecting fault: "<>) . show

testLogFInject :: (CanLog m, HasCallStack, HasLoggerName m, MonadIO m)
           => FInjects -> FInject -> m Bool
testLogFInject fis fi = do
  injecting <- testFInject fis fi
  logFInject fi
  pure injecting
