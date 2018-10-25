{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
       , listFInjects
       ) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Options.Applicative

import           Pos.Util.Wlog (CanLog, HasLoggerName, logError, logWarning,
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

type    FInjectsSpec   = Maybe (Set FInject)
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

newtype FInjectsHandle = FInjectsHandle { _fromFInjsHandle :: Maybe (IORef (Set FInject)) }

data FInjects m     = FInjects
    { setFInject     :: FInject -> Bool -> m ()
    , testLogFInject :: FInject -> m Bool
    , listFInjects   :: m [FInject]
    }

logWarningLoud :: (CanLog m, HasLoggerName m) => String -> m ()
logWarningLoud s = do
  logWarning $ T.unlines
    [ "***"
    , "*** " <> T.pack s
    , "***" ]

-- | Make a stateful fault injection configuration object.
mkFInjects :: (CanLog m, HasCallStack, HasLoggerName m, MonadIO m) => FInjectsSpec -> m (FInjects m)
mkFInjects mfs = do
  handle@(FInjectsHandle h) <- FInjectsHandle <$> (sequence $ newIORef <$> mfs)
  when (isJust h) $
    logWarningLoud "FAULT INJECTION MACHINERY ACTIVE, ALL WARRANTIES VOID"
  pure $ FInjects
    { setFInject     = mkSetFInject     handle
    , testLogFInject = mkTestLogFInject handle
    , listFInjects   = mkListFInjects   handle
    }

-- | Test if code holding a reference, wants a particular fault injection enabled.
testFInject :: MonadIO m => FInjectsHandle -> FInject -> m Bool
testFInject (FInjectsHandle Nothing) _ = pure False
testFInject (FInjectsHandle (Just fsRef)) fi =
  Set.member fi <$> readIORef fsRef
{-# INLINE testFInject #-}

-- | Signal enablement of particular fault injection to listeners.
mkSetFInject :: (CanLog m, HasLoggerName m, MonadIO m) => FInjectsHandle -> FInject -> Bool -> m ()
mkSetFInject  (FInjectsHandle Nothing) _ _ = pure ()
mkSetFInject  (FInjectsHandle (Just fsRef)) fi enable = do
  if enable
  then logWarningLoud $ "EXPECT MISCHIEF -- ENABLING INJECTION OF FAULT " <> show fi
  else logWarning     $ "disabling injection of fault " <> show fi
  modifyIORef' fsRef (if enable then Set.insert fi else Set.delete fi)

logFInject :: (CanLog m, HasCallStack, HasLoggerName m)
           => FInject -> m ()
logFInject = modifyLoggerName (const "InjectFail") .
  logError . T.pack . (<> prettyCallStack callStack) . ("injecting fault: "<>) . show

mkTestLogFInject :: (CanLog m, HasCallStack, HasLoggerName m, MonadIO m)
                 => FInjectsHandle -> FInject -> m Bool
mkTestLogFInject fis fi = do
  injecting <- testFInject fis fi
  when injecting $
    logFInject fi
  pure injecting

mkListFInjects :: forall m. (MonadIO m)
               => FInjectsHandle -> m [FInject]
mkListFInjects fis = fromMaybe [] <$> (traverse (fmap Set.toList <$> readIORef) (_fromFInjsHandle fis))
