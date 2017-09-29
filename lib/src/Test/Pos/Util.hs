{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Test.Pos.Util
       ( HasStaticConfigurations
       , withDefConfiguration
       , withDefInfraConfiguration
       , withDefNodeConfiguration
       , withDefGtConfiguration
       , withDefUpdateConfiguration
       , withDefConfigurations
       , withStaticConfigurations

       -- * Various properties and predicates
       , qcIsJust
       , qcIsNothing
       , qcIsLeft
       , qcIsRight
       , qcElem
       , qcNotElem
       , qcFail

       -- * Monadic properties
       , stopProperty
       , maybeStopProperty
       , splitIntoChunks
       ) where

import           Universum

import           Data.Tagged                      (Tagged (..))
import           Test.QuickCheck                  (Arbitrary (arbitrary), Property,
                                                   counterexample, property)
import           Test.QuickCheck.Gen              (choose)
import           Test.QuickCheck.Monadic          (PropertyM, pick, stop)
import           Test.QuickCheck.Property         (Result (..), failed)

import           Pos.Configuration                (HasNodeConfiguration,
                                                   withNodeConfiguration)
import           Pos.Core                         (HasConfiguration, withGenesisSpec)
import           Pos.Infra.Configuration          (HasInfraConfiguration,
                                                   withInfraConfiguration)
import           Pos.Launcher.Configuration       (Configuration (..), HasConfigurations)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration,
                                                   withGtConfiguration)
import           Pos.Update.Configuration         (HasUpdateConfiguration,
                                                   withUpdateConfiguration)

import           Test.Pos.Configuration           (defaultTestConf)

-- | This constraint requires all configurations which are not
-- always hardcoded in tests (currently).
type HasStaticConfigurations =
    ( HasInfraConfiguration
    , HasUpdateConfiguration
    , HasGtConfiguration
    , HasNodeConfiguration
    )

withDefNodeConfiguration :: (HasNodeConfiguration => r) -> r
withDefNodeConfiguration = withNodeConfiguration (ccNode defaultTestConf)

withDefGtConfiguration :: (HasGtConfiguration => r) -> r
withDefGtConfiguration = withGtConfiguration (ccGt defaultTestConf)

withDefUpdateConfiguration :: (HasUpdateConfiguration => r) -> r
withDefUpdateConfiguration = withUpdateConfiguration (ccUpdate defaultTestConf)

withDefInfraConfiguration :: (HasInfraConfiguration => r) -> r
withDefInfraConfiguration = withInfraConfiguration (ccInfra defaultTestConf)

withDefConfiguration :: (HasConfiguration => r) -> r
withDefConfiguration = withGenesisSpec 0 (ccCore defaultTestConf)

withStaticConfigurations :: (HasStaticConfigurations => r) -> r
withStaticConfigurations patak =
    withDefNodeConfiguration $
    withDefGtConfiguration $
    withDefUpdateConfiguration $
    withDefInfraConfiguration patak

withDefConfigurations :: (HasConfigurations => r) -> r
withDefConfigurations bardaq =
    withDefConfiguration $ withStaticConfigurations bardaq

instance Arbitrary a => Arbitrary (Tagged s a) where
    arbitrary = Tagged <$> arbitrary

----------------------------------------------------------------------------
-- Various properties and predicates
----------------------------------------------------------------------------

qcIsJust :: Maybe a -> Property
qcIsJust (Just _) = property True
qcIsJust Nothing  = qcFail "expected Just, got Nothing"

qcIsNothing :: Show a => Maybe a -> Property
qcIsNothing Nothing  = property True
qcIsNothing (Just x) = qcFail ("expected Nothing, got Just (" <> show x <> ")")

qcIsLeft :: Show b => Either a b -> Property
qcIsLeft (Left _)  = property True
qcIsLeft (Right x) = qcFail ("expected Left, got Right (" <> show x <> ")")

qcIsRight :: Show a => Either a b -> Property
qcIsRight (Right _) = property True
qcIsRight (Left x)  = qcFail ("expected Right, got Left (" <> show x <> ")")

qcElem
    :: (Eq a, Show a, Show t, NontrivialContainer t, Element t ~ a)
    => a -> t -> Property
qcElem x xs =
    counterexample ("expected " <> show x <> " to be in " <> show xs) $
    x `elem` xs

qcNotElem
    :: (Eq a, Show a, Show t, NontrivialContainer t, Element t ~ a)
    => a -> t -> Property
qcNotElem x xs =
    counterexample ("expected " <> show x <> " not to be in " <> show xs) $
    not (x `elem` xs)

-- | A property that is always false
qcFail :: Text -> Property
qcFail s = counterexample (toString s) False

----------------------------------------------------------------------------
-- Monadic testing
----------------------------------------------------------------------------

-- Note, 'fail' does the same thing, but:
-- • it's quite trivial, almost no copy-paste;
-- • it's 'fail' from 'Monad', not 'MonadFail';
-- • I am not a fan of 'fail'.
-- | Stop 'PropertyM' execution with given reason. The property will fail.
stopProperty :: Monad m => Text -> PropertyM m a
stopProperty msg = stop failed {reason = toString msg}

-- | Use 'stopProperty' if the value is 'Nothing' or return something
-- it the value is 'Just'.
maybeStopProperty :: Monad m => Text -> Maybe a -> PropertyM m a
maybeStopProperty msg =
    \case
        Nothing -> stopProperty msg
        Just x -> pure x

-- | Split given list into chunks with size up to given value.
splitIntoChunks :: Monad m => Word -> [a] -> PropertyM m [NonEmpty a]
splitIntoChunks 0 _ = error "splitIntoChunks: maxSize is 0"
splitIntoChunks maxSize items = do
    sizeMinus1 <- pick $ choose (0, maxSize - 1)
    let (chunk, rest) = splitAt (fromIntegral sizeMinus1 + 1) items
    case nonEmpty chunk of
        Nothing      -> return []
        Just chunkNE -> (chunkNE :) <$> splitIntoChunks maxSize rest
