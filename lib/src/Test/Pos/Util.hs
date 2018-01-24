{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Test.Pos.Util
       ( HasStaticConfigurations
       , withDefConfiguration
       , withDefInfraConfiguration
       , withDefNodeConfiguration
       , withDefSscConfiguration
       , withDefUpdateConfiguration
       , withDefBlockConfiguration
       , withDefDlgConfiguration
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
       , assertProperty
       , stopProperty
       , maybeStopProperty
       , splitIntoChunks
       , expectedOne

       -- * HSpec utils
       , expectationError

       -- * Generators
       , splitWord
       , sumEquals
       ) where

import           Universum

import           Data.Tagged (Tagged (..))
import           Test.QuickCheck (Arbitrary (arbitrary), Property, counterexample, property)
import           Test.QuickCheck.Gen (Gen, choose)
import           Test.QuickCheck.Monadic (PropertyM, pick, stop)
import           Test.QuickCheck.Property (Result (..), failed)
import qualified Test.Hspec as Hspec (Expectation)

import           Pos.Block.Configuration (HasBlockConfiguration, withBlockConfiguration)
import           Pos.Configuration (HasNodeConfiguration, withNodeConfiguration)
import           Pos.Core (HasConfiguration, withGenesisSpec)
import           Pos.Delegation (HasDlgConfiguration, withDlgConfiguration)
import           Pos.Infra.Configuration (HasInfraConfiguration, withInfraConfiguration)
import           Pos.Launcher.Configuration (Configuration (..), HasConfigurations)
import           Pos.Ssc.Configuration (HasSscConfiguration, withSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration, withUpdateConfiguration)

import           Test.Pos.Configuration (defaultTestConf)

-- | This constraint requires all configurations which are not
-- always hardcoded in tests (currently).
type HasStaticConfigurations =
    ( HasInfraConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasBlockConfiguration
    , HasNodeConfiguration
    , HasDlgConfiguration
    )

withDefNodeConfiguration :: (HasNodeConfiguration => r) -> r
withDefNodeConfiguration = withNodeConfiguration (ccNode defaultTestConf)

withDefSscConfiguration :: (HasSscConfiguration => r) -> r
withDefSscConfiguration = withSscConfiguration (ccSsc defaultTestConf)

withDefUpdateConfiguration :: (HasUpdateConfiguration => r) -> r
withDefUpdateConfiguration = withUpdateConfiguration (ccUpdate defaultTestConf)

withDefInfraConfiguration :: (HasInfraConfiguration => r) -> r
withDefInfraConfiguration = withInfraConfiguration (ccInfra defaultTestConf)

withDefBlockConfiguration :: (HasBlockConfiguration => r) -> r
withDefBlockConfiguration = withBlockConfiguration (ccBlock defaultTestConf)

withDefDlgConfiguration :: (HasDlgConfiguration => r) -> r
withDefDlgConfiguration = withDlgConfiguration (ccDlg defaultTestConf)

withDefConfiguration :: (HasConfiguration => r) -> r
withDefConfiguration = withGenesisSpec 0 (ccCore defaultTestConf)

withStaticConfigurations :: (HasStaticConfigurations => r) -> r
withStaticConfigurations patak =
    withDefNodeConfiguration $
    withDefSscConfiguration $
    withDefUpdateConfiguration $
    withDefBlockConfiguration $
    withDefDlgConfiguration $
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

-- | Call stopProperty if boolean value is false.
assertProperty :: Monad m => Bool -> Text -> PropertyM m ()
assertProperty st text = unless st $ stopProperty text

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
-- TODO: consider using `sumEquals maxSize (length items)`
splitIntoChunks :: Monad m => Word -> [a] -> PropertyM m [NonEmpty a]
splitIntoChunks 0 _ = error "splitIntoChunks: maxSize is 0"
splitIntoChunks maxSize items = do
    sizeMinus1 <- pick $ choose (0, maxSize - 1)
    let (chunk, rest) = splitAt (fromIntegral sizeMinus1 + 1) items
    case nonEmpty chunk of
        Nothing      -> return []
        Just chunkNE -> (chunkNE :) <$> splitIntoChunks maxSize rest

expectedOne :: Monad m => Text -> [a] -> PropertyM m a
expectedOne desc = \case
    [] ->  kickOut "expected exactly one element, but list is empty"
    [x] -> pure x
    _ ->   kickOut "expected exactly one element, but list contains more elements"
  where
    kickOut err = stopProperty $ err <> " (" <> desc <> ")"

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

-- | Split given integer `total` into `parts` parts
-- TODO: improve naming!
splitWord :: Word64 -> Word64 -> Gen [Word64]
splitWord total parts | total < parts =
    error $ "splitWord: can't split " <> show total <> " into " <> show parts <> " parts."
                      | otherwise = map succ . take iParts <$> ((<> replicate iParts 0) <$> (sumEquals (total `div` parts + 1) $ total - parts))
  where
    iParts = fromIntegral parts

-- | Generate list of arbitrary positive integers which sum equals given sum.
-- All elements in the list will be smaller or equal then first parameter
sumEquals :: Word64 -> Word64 -> Gen [Word64]
sumEquals 0 _ = pure []
sumEquals _ 0 = pure []
sumEquals maxEl restSum = do
    el <- choose (1, min maxEl restSum)
    (el:) <$> sumEquals maxEl (restSum - el)

expectationError :: Text -> Hspec.Expectation
expectationError = fail . toString
