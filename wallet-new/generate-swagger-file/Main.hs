{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Universum

import           Data.Swagger (Swagger)
import           Options.Applicative

import           Cardano.Wallet.API (devAPI, v0API, v1API)
import           Pos.Core (ApplicationName (..), SoftwareVersion (..))
import           Pos.Util.CompileInfo (CompileTimeInfo, retrieveCompileTimeInfo)

import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL


data Command = Command
  { targetAPI  :: TargetAPI
  , outputFile :: Maybe FilePath
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
            <$> targetAPIOption (short 't' <> long "target" <> metavar "API"
                  <> help "Target API with version (e.g. 'wallet@v1', 'wallet@v0', 'wallet@dev'...)")

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

        let filename = fromMaybe defaultOutputFilename outputFile

        BL.writeFile filename . Aeson.encode $ mkSwagger softwareDetails targetAPI


mkSwagger :: (CompileTimeInfo, SoftwareVersion) -> TargetAPI -> Swagger
mkSwagger details = \case
    TargetWalletDev ->
        Swagger.api details devAPI Swagger.highLevelShortDescription
    TargetWalletV0 ->
        Swagger.api details v0API  Swagger.highLevelShortDescription
    TargetWalletV1  ->
        Swagger.api details v1API  Swagger.highLevelDescription


-- NOTE The software version is hard-coded here. Do determine the SoftwareVersion,
-- we'd have to pull curSoftwareVersion in a `HasUpdateConfiguration` context
-- which require parsing a configuration file. This only to get a more-or-less
-- accurate string somewhere in the Swagger description about the software
-- version (e.g. cardano-sl:0). See previous commit for an example on how to
-- this if needed but for the sake of this tools, it only clutters its scope.
softwareDetails :: (CompileTimeInfo, SoftwareVersion)
softwareDetails =
    ( $(retrieveCompileTimeInfo)
    , SoftwareVersion (ApplicationName "cardano-sl") 1
    )
