{-# LANGUAGE CPP #-}

-- | This is a separate module due to the TH stage restriction
module Pos.Util.Config.Get
       ( getCslConfig
       ) where

import           Universum

import qualified Data.HashMap.Strict  as HM
import qualified Data.Yaml            as Y

import           Pos.Util.Config.Path (cslConfigFilePath)

#define QUOTED(x) "/**/x/**/"

#if !defined(CONFIG)

# error CPP variable CONFIG isn't defined (should be 'dev', 'prod' or 'wallet'). If you're building with Stack, pass --ghc-options=-DCONFIG=..., or consider using build.sh instead

#else

-- | Parse @constants.yaml@ and pick the right config according to @CONFIG@.
getCslConfig :: IO (Either String Y.Value)
getCslConfig = do
    mbRes <- Y.decodeFileEither @Y.Object cslConfigFilePath
    pure $ do
        val <- case mbRes of
            Right x  -> pure x
            Left err -> Left $
                "Couldn't parse " ++ cslConfigFilePath ++ ": " ++
                Y.prettyPrintParseException err
        case HM.lookup QUOTED(CONFIG) val of
            Just x  -> pure x
            Nothing -> Left $
                "Couldn't find config " ++ show (QUOTED(CONFIG) :: Text) ++
                " in " ++ cslConfigFilePath
#endif
