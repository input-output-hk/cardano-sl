-- | Compile-time genesis data parser

module Pos.Core.Genesis.Parser
       ( defaultGenesisSpec
       , genesisSpec
       , setGenesisSpec
       , setGenesisSpecFromFile
       ) where

import           Universum

import qualified Data.ByteString        as BS
import           Data.FileEmbed         (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict    as HM
import           Data.List              (stripPrefix)
import           Data.Yaml              (decodeEither)
import qualified Language.Haskell.TH    as TH
import           System.Directory       (listDirectory)
import           System.FilePath        (takeBaseName, takeExtension, (</>))
import           System.IO.Unsafe       (unsafePerformIO)

import           Pos.Aeson.Genesis      ()
import           Pos.Core.Constants     (genesisBinSuffix)
import           Pos.Core.Genesis.Types (GenesisSpec)
import           Pos.Util.Future        (newInitFuture)

-- | Pre-generated genesis data from /genesis-spec.yaml/ for all genesis
-- files (i.e. /genesis-spec-qa.bin/, /genesis-spec-tns.bin/, etc). Each key
-- in the map is a prefix (“qa”, “tns”, etc).
allGenesisSpecs :: HashMap String GenesisSpec
allGenesisSpecs =
    let bins :: [(String, FilePath, ByteString)]
        bins = $(do
            dir <- makeRelativeToProject ""
            let getSuffix fp = do
                    guard (takeExtension fp == ".yaml")
                    stripPrefix "genesis-spec-" (takeBaseName fp)
            suffs <- mapMaybe getSuffix <$> TH.runIO (listDirectory dir)
            let paths = suffs <&> \suff ->
                    dir </> ("genesis-spec-" <> suff <> ".yaml")
            TH.listE $ zip suffs paths <&> \(suff, path) ->
                [|(suff, path, $(embedFile path))|]
            )
    in HM.fromList $ bins <&> \(suff, path, file) ->
           case decodeEither file of
               Left err -> error $ "Failed to read genesis-spec from " <>
                                   toText path <> ": " <> toText err
               Right d  -> (suff, d)

defaultGenesisSpec :: GenesisSpec
defaultGenesisSpec =
    case HM.lookup genesisBinSuffix allGenesisSpecs of
        Just gd -> gd
        Nothing -> error $ "The config says that the genesis-spec.yaml suffix \
                           \should be " <> toText genesisBinSuffix <>
                           ", but corresponding file was not found in \
                           \'allGenesisSpecs'"

genesisSpec :: GenesisSpec
setGenesisSpec :: GenesisSpec -> IO ()
(genesisSpec, setGenesisSpec) =
    unsafePerformIO (newInitFuture "genesisSpec")
{-# NOINLINE genesisSpec #-}
{-# NOINLINE setGenesisSpec #-}

setGenesisSpecFromFile :: FilePath -> IO ()
setGenesisSpecFromFile path = do
    bs <- BS.readFile path
    case decodeEither bs of
        Left err -> error $ "Failed to read genesis spec from " <>
                            toText path <> ": " <> toText err
        Right gd -> setGenesisSpec gd
