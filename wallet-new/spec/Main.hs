-- | Swagger spec generation.

module Main
    ( main
    ) where

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Version (showVersion)
import           Options.Applicative (execParser, footer, fullDesc, header, help, helper,
                                      infoOption, long, progDesc)
import qualified Options.Applicative as Opt
import           Universum

import qualified Paths_cardano_sl as CSL

import qualified Cardano.Wallet.API.V1.Swagger as Swagger

import           SwaggerGenOptions (SwaggerGenOptions (..), swaggerGenOptionsParser)


getSwaggerGenOptions :: IO SwaggerGenOptions
getSwaggerGenOptions = execParser programInfo
  where
    programInfo = Opt.info (helper <*> versionOption <*> swaggerGenOptionsParser) $
        fullDesc <> progDesc "Generate Swagger specification for Wallet web API."
                 <> header   "Cardano SL Wallet web API docs generator."
                 <> footer   ""

    versionOption = infoOption
        ("cardano-swagger-" <> showVersion CSL.version)
        (long "version" <> help "Show version.")

main :: IO ()
main = do
    SwaggerGenOptions{..} <- getSwaggerGenOptions
    let spec = encodePretty Swagger.api
    if sgoDumpToConsole
        then BSL8.putStr spec
        else do BSL8.writeFile sgoFile spec
                putText $ "Done. See " <> toText sgoFile <> "."
