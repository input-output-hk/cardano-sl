{-# LANGUAGE CPP #-}

-- | This is a separate module due to the TH stage restriction
module Pos.Util.Config.Get
       ( getCslConfig
       , cslConfigName
       ) where

import           Universum            hiding (die)

#ifdef NO_EMBED
import           System.Environment   (lookupEnv)
import           System.Exit          (die)
import           System.IO.Unsafe     (unsafePerformIO)
#endif

import qualified Data.HashMap.Strict  as HM
import qualified Data.Yaml            as Y

import           Pos.Util.Config.Path (cslConfigPath)


#if defined(NO_EMBED)

cslConfigName :: Text
cslConfigName = unsafePerformIO $
    lookupEnv "CSL_CONFIG_NAME" >>= \case
        Just x  -> pure (toText x)
        Nothing -> die "Error: when compiled with 'no-embed', the \
                       \'CSL_CONFIG_NAME' environment variable should be \
                       \present to provide the name of the section of \
                       \constants.yaml that will be used as the config"
{-# NOINLINE cslConfigName #-}

#elif defined(CONFIG)

#define QUOTED(x) "/**/x/**/"

cslConfigName :: Text
cslConfigName = QUOTED(CONFIG)

#else

# error CPP variable CONFIG isn't defined. If you're building with Stack, pass --ghc-options=-DCONFIG=..., or consider using scripts/build/cardano-sl.sh instead. Alternatively, perhaps you wanted to compile with the 'no-embed' flag?

#endif

-- | Parse @constants.yaml@ and pick the right config according to @CONFIG@.
getCslConfig :: IO (Either String Y.Value)
getCslConfig = do
    mbRes <- Y.decodeFileEither @Y.Object cslConfigPath
    pure $ do
        val <- case mbRes of
            Right x  -> pure x
            Left err -> Left $
                "Couldn't parse " ++ cslConfigPath ++ ": " ++
                Y.prettyPrintParseException err
        case HM.lookup cslConfigName val of
            Just x  -> pure x
            Nothing -> Left $
                "Couldn't find config " ++ show cslConfigName ++
                " in " ++ cslConfigPath
