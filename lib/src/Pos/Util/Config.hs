-- | Config helpers

module Pos.Util.Config
       ( embedYamlConfigCT
       , embedYamlObject
       , parseYamlConfig
       , ConfigurationException (..)
       ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.Yaml as Y
import qualified Language.Haskell.TH.Syntax as TH
import           System.Directory (canonicalizePath, getDirectoryContents)
import           System.FilePath (takeDirectory, takeFileName, (</>))

import           Pos.Util.Util (maybeThrow, templateHaskellError)

embedYamlObject :: Y.FromJSON r => FilePath -> FilePath -> (r -> TH.Q TH.Exp) -> TH.Q TH.Exp
embedYamlObject name marker parser = do
    -- This code was stolen from file-embed ('makeRelativeToProject'). We
    -- don't use file-embed because the config-finding logic has already been
    -- changed several times and switching from file-embed to custom logic
    -- and back is annoying.
    let findConfigDir x = do
         let dir = takeDirectory x
         contents <- getDirectoryContents dir
         let isRoot = any ((== marker) . takeFileName) contents
         if | dir == x  -> return Nothing
            | isRoot    -> return (Just dir)
            | otherwise -> findConfigDir dir
    loc <- TH.qLocation
    path <- TH.runIO $ do
        srcFP <- canonicalizePath $ TH.loc_filename loc
        mdir <- findConfigDir srcFP
        case mdir of
            Just dir -> return (dir </> name)
            Nothing  -> error $ toText $
                "Could not find " ++ marker ++ " for path: " ++ srcFP
    TH.qAddDependentFile path
    TH.runIO (Y.decodeFileEither path) >>= \case
        Right x  -> parser x
        Left err -> templateHaskellError $
            "Couldn't parse " <> pretty path <> ": " <>
            fromString (Y.prettyPrintParseException err)

embedYamlConfigCT :: forall conf . (Y.FromJSON conf, TH.Lift conf)
                => Proxy conf -> FilePath -> FilePath -> Text -> TH.Q TH.Exp
embedYamlConfigCT _ name marker key =
    embedYamlObject @(Map Text conf) name marker $ \multiConfig ->
        case Map.lookup key multiConfig of
            Just a -> TH.lift a
            Nothing -> templateHaskellError $
                "Embedded file " <> fromString name <> " contains no key " <> key

parseYamlConfig :: (MonadThrow m, MonadIO m, Y.FromJSON conf)
            => FilePath -> Text -> m conf
parseYamlConfig cfoFilePath cfoKey = do
    decoded <- liftIO $ Y.decodeFileEither cfoFilePath
    multiConfig <- either (throwM . ConfigurationParseFailure cfoFilePath) return decoded
    maybeThrow (ConfigurationKeyNotFound cfoFilePath cfoKey)
               (Map.lookup cfoKey multiConfig)

data ConfigurationException =

      -- | Couldn't parse the configuration file.
      ConfigurationParseFailure !FilePath !(Y.ParseException)

      -- | Configuration at the given key not found.
    | ConfigurationKeyNotFound !FilePath !Text

    deriving (Show)

instance Exception ConfigurationException
