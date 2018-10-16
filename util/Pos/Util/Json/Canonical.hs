{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Canonical encoding of 'GenesisData'.

module Pos.Util.Json.Canonical
       ( SchemaError (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import qualified Data.Text.Buildable as Buildable
import qualified Data.Text.Lazy.Builder as Builder (fromText)
import           Text.JSON.Canonical (FromJSON (..), JSValue (..), ToJSON (..),
                                      ReportSchemaErrors (expected), expectedButGotValue)

data SchemaError = SchemaError
    { seExpected :: !Text
    , seActual   :: !(Maybe Text)
    } deriving (Show)

instance Buildable SchemaError where
    build se = mconcat
        [ "expected " <> Builder.fromText (seExpected se)
        , case seActual se of
            Nothing     -> mempty
            Just actual -> " but got " <> Builder.fromText actual
        ]

instance (Monad m, Applicative m, MonadError SchemaError m) => ReportSchemaErrors m where
    expected expec actual = throwError SchemaError
        { seExpected = fromString expec
        , seActual = fmap fromString actual
        }

instance Monad m => ToJSON m Int32 where
    toJSON = pure . JSNum . fromIntegral

instance (ReportSchemaErrors m) => FromJSON m Int32 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Int32" val
