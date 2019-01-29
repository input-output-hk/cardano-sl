{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Util.Tripping where

import qualified Prelude
import           Universum

import           Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Text.Internal.Builder (fromText, toLazyText)
import           Formatting.Buildable (Buildable (..))
import           Hedgehog (Group, MonadTest, discoverPrefix, success, tripping)
import           Hedgehog.Internal.Property (Diff (..), failWith)
import           Hedgehog.Internal.Show (valueDiff)
import           Hedgehog.Internal.TH (TExpQ)
import           System.IO (hSetEncoding, stderr, stdout, utf8)
import qualified Text.JSON.Canonical as Canonical
import           Text.Show.Pretty (Value (..), parseValue)

import           Pos.Util.Json.Canonical (SchemaError (..))

discoverRoundTrip :: TExpQ Group
discoverRoundTrip = discoverPrefix "roundTrip"

roundTripsAesonShow
    :: (Eq a, MonadTest m, ToJSON a, FromJSON a, Show a) => a -> m ()
roundTripsAesonShow a = tripping a encode eitherDecode

-- | Round trip any `a` with both `ToJSON` and `FromJSON` instances
roundTripsAesonBuildable
    :: (Eq a, MonadTest m, ToJSON a, FromJSON a, Buildable a) => a -> m ()
roundTripsAesonBuildable a = trippingBuildable a encode eitherDecode

-- We want @SchemaError@s to show up different (register failure)
instance Eq SchemaError where
    _ == _ = False

roundTripsCanonicalJSONShow
    :: forall m a
     . ( Eq a
       , MonadTest m
       , Canonical.ToJSON Identity a
       , Canonical.FromJSON (Either SchemaError) a
       , HasCallStack
       , Show a
       )
    => a
    -> m ()
roundTripsCanonicalJSONShow x =
    tripping x (runIdentity . Canonical.toJSON :: a -> Canonical.JSValue)
               (Canonical.fromJSON :: Canonical.JSValue -> Either SchemaError a)

runTests :: [IO Bool] -> IO ()
runTests tests' = do
    -- ensure UTF-8. As that's what hedgehog needs.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    result <- and <$> sequence tests'
    unless result
        exitFailure

-- | Round trip using given encode and decode functions for types with a
--   `Buildable` instance
trippingBuildable :: (Buildable (f a), Eq (f a), Show b, Applicative f, MonadTest m) => a -> (a -> b) -> (b -> f a) -> m ()
trippingBuildable x enc dec =
  let mx = pure x
      i = enc x
      my = dec i
  in if mx == my
        then success
        else case valueDiff <$> buildValue mx <*> buildValue my of
            Nothing ->
                withFrozenCallStack $
                    failWith Nothing $ Prelude.unlines
                        [ "━━━ Original ━━━"
                        , buildPretty mx
                        , "━━━ Intermediate ━━━"
                        , show i
                        , "━━━ Roundtrip ━━━"
                        , buildPretty my
                        ]

            Just diff ->
                withFrozenCallStack $
                    failWith
                        (Just $ Diff "━━━ " "- Original" "/" "+ Roundtrip" " ━━━" diff) $
                            Prelude.unlines
                            [ "━━━ Intermediate ━━━"
                            , show i
                            ]

instance Buildable a => Buildable (Either Text a) where
    build (Left t)  = fromText t
    build (Right a) = build a

instance Buildable a => Buildable (Either String a) where
    build (Left t)  = fromString t
    build (Right a) = build a

instance Buildable () where
    build () = "()"

buildPretty :: Buildable a => a -> String
buildPretty = show . buildValue

buildValue :: Buildable a => a -> Maybe Value
buildValue = parseValue . stringBuild

stringBuild :: Buildable a => a -> String
stringBuild = toString . toLazyText  . build
