{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Fault injection, strategically placed at the root of CSL dependency graph.

module Pos.Infra.InjectFail
       ( FInjects, FInjectsSpec
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
                     logWarning, modifyLoggerName)

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

newtype FInjects     = FInjects { _fromFInjs :: Maybe (IORef (Set FInject)) }
type    FInjectsSpec = Maybe (Set FInject)
-- ^ Configuration of fault injection: 'Nothing' means completely disabled,
-- whereas 'Just mempty' means starting with no injections enabled.

parseFInjectsSpec :: Parser FInjectsSpec
parseFInjectsSpec = do
  finjs <- Set.fromList . catMaybes <$> sequenceA (parseSingle <$> enumFromTo minBound maxBound)
  allow <- switch (long "allow-fault-injection" <> help "Allow fault injection processing" )
  pure $ if allow
         then Just finjs
         else Nothing

parseSingle :: FInject -> Parser (Maybe FInject)
parseSingle fi =
    flag Nothing (Just fi) opt
  where
    opt = long (drop 4 $ map toLower $ show fi) <> help (desc fi)

-- | Make a stateful fault injection configuration object.
mkFInjects :: (CanLog m, HasLoggerName m, MonadIO m) => FInjectsSpec -> m FInjects
mkFInjects Nothing   = pure $ FInjects Nothing
mkFInjects (Just fs) = do
  logWarning "***"
  logWarning "*** FAULT INJECTION MACHINERY ACTIVE, ALL WARRANTIES VOID"
  logWarning "***"
  FInjects . Just <$> liftIO (newIORef fs)

-- | Test if code holding a reference, wants a particular fault injection enabled.
testFInject :: MonadIO m => FInjects -> FInject -> m Bool
testFInject (FInjects Nothing) _ = pure False
testFInject (FInjects (Just fsRef)) fi =
  Set.member fi <$> readIORef fsRef
{-# INLINE testFInject #-}

-- | Signal enablement of particular fault injection to listeners.
setFInject :: MonadIO m => FInjects -> FInject -> Bool -> m ()
setFInject  (FInjects Nothing) _ _ = pure ()
setFInject  (FInjects (Just fsRef)) fi enable =
  modifyIORef' fsRef (if enable then Set.insert fi else Set.delete fi)

logFInject :: (CanLog m, HasCallStack, HasLoggerName m)
           => FInject -> m ()
logFInject = modifyLoggerName (const "InjectFail") .
  logError . T.pack . (<> prettyCallStack callStack) . ("injecting fault: "<>) . show

testLogFInject :: (CanLog m, HasCallStack, HasLoggerName m, MonadIO m)
           => FInjects -> FInject -> m Bool
testLogFInject fis fi = do
  injecting <- testFInject fis fi
  when injecting $
    logFInject fi
  pure injecting
