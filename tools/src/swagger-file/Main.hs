{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Universum

import           Data.Swagger (Swagger)
import           Options.Applicative

import           Cardano.Wallet.API (devAPI, v0API, v1API)
import           Pos.Core (SoftwareVersion)
import           Pos.Launcher.Configuration (Configuration (..), ConfigurationOptions (..))
import           Pos.Update.Configuration (curSoftwareVersion, withUpdateConfiguration)
import           Pos.Util.CompileInfo (CompileTimeInfo, retrieveCompileTimeInfo)
import           Pos.Util.Config (parseYamlConfig)

import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL


data Command = Command
  { targetAPI            :: TargetAPI
  , configurationOptions :: ConfigurationOptions
  , outputFile           :: Maybe FilePath
  } deriving (Show)


data TargetAPI
    = TargetWalletV1
    | TargetWalletV0
    | TargetWalletDev
    deriving (Show)


defaultOutputFilename :: FilePath
defaultOutputFilename = "swagger.json"


main :: IO ()
main =
    let
        opts :: ParserInfo Command
        opts =
            info (cmdParser <**> helper)
                 (fullDesc
                 <> header "Swagger File"
                 <> progDesc "Output Swagger spec (2.0) file corresponding to the given API"
                 )

        cmdParser :: Parser Command
        cmdParser = Command
            <$> targetAPIOption (long "api" <> metavar "API"
                  <> help "Target API with version (e.g. 'wallet@v1', 'wallet@v0', 'wallet@dev'...)")

            <*> (ConfigurationOptions
                <$> strOption (short 'c' <> long "configuration-file" <> metavar "FILEPATH"
                    <> help "Configuration file containing for the node")

                <*> fmap toText (strOption (short 'k' <> long "configuration-key" <> metavar "KEY"
                    <> help "Configuration key within the config file (e.g. 'dev', 'test'...)"))

                <*> pure Nothing

                <*> pure Nothing
                )

            <*> optional (strOption (short 'o' <> long "output-file" <> metavar "FILEPATH"
                    <> help ("Output file, default to: " <> defaultOutputFilename)))

        targetAPIOption :: Mod OptionFields TargetAPI -> Parser TargetAPI
        targetAPIOption = option $ maybeReader $ \case
            "wallet@v0"  -> Just TargetWalletV0
            "wallet@v1"  -> Just TargetWalletV1
            "wallet@dev" -> Just TargetWalletDev
            _            -> Nothing
    in do
        Command{..} <-
            execParser opts

        swagger <-
            mkSwagger <$> getSoftwareDetails configurationOptions <*> pure targetAPI

        BL.writeFile (fromMaybe defaultOutputFilename outputFile) . Aeson.encode $ swagger


mkSwagger :: (CompileTimeInfo, SoftwareVersion) -> TargetAPI -> Swagger
mkSwagger details = \case
    TargetWalletDev ->
        Swagger.api details devAPI Swagger.highLevelShortDescription
    TargetWalletV0 ->
        Swagger.api details v0API  Swagger.highLevelShortDescription
    TargetWalletV1  ->
        Swagger.api details v1API  Swagger.highLevelDescription


getSoftwareDetails :: ConfigurationOptions -> IO (CompileTimeInfo, SoftwareVersion)
getSoftwareDetails ConfigurationOptions{..} = do
    Configuration{..} <- parseYamlConfig cfoFilePath cfoKey
    return $ withUpdateConfiguration ccUpdate $
        ($(retrieveCompileTimeInfo), curSoftwareVersion)
