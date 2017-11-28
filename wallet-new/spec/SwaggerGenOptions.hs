-- | Command line options of swagger generator.

module SwaggerGenOptions
    ( SwaggerGenOptions (..)
    , swaggerGenOptionsParser
    ) where

import qualified Options.Applicative as Opt
import           Universum

import qualified Cardano.Wallet.API.V1.Swagger as Swagger

data SwaggerGenOptions = SwaggerGenOptions
    { sgoFile          :: FilePath
    , sgoDumpToConsole :: Bool
    }

swaggerGenOptionsParser :: Opt.Parser SwaggerGenOptions
swaggerGenOptionsParser =
    SwaggerGenOptions
        <$> sgoFileParser
        <*> sgoDumpToConsoleParser
  where
    sgoFileParser = Opt.strOption $
        Opt.long "output" <>
        Opt.short 'o' <>
        Opt.metavar "FILEPATH" <>
        Opt.help "Path to file where write swagger spec to" <>
        Opt.value Swagger.specFile

    sgoDumpToConsoleParser = Opt.switch $
        Opt.long "dump" <>
        Opt.help "Prints doc to console instead of writing to file"


