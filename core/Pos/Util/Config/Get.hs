{-# LANGUAGE CPP #-}

-- | This is a separate module due to the TH stage restriction
module Pos.Util.Config.Get
       ( getCslConfig
       , cslConfigName
       ) where

import           Universum

import qualified Data.HashMap.Strict  as HM
import qualified Data.Yaml            as Y
#if defined(CONFIG)


import           Pos.Util.Config.Path (cslConfigPath)

#define QUOTED(x) "/**/x/**/"

#if defined(CONFIG)
cslConfigName :: Text
cslConfigName = QUOTED(CONFIG)
#else
cslConfigName :: Text
cslConfigName = unsafePerformIO $ toText <$> getEnv "CSL_CONFIG_NAME"
{-# NOINLINE cslConfigName #-}
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
#endif
