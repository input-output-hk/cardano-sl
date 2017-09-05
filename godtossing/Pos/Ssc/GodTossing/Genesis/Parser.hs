module Pos.Ssc.GodTossing.Genesis.Parser
       ( allGenGtDatas
       , defaultGenGtData
       , genGtData
       , setGenGtData
       , setGenGtDataFromFile
       ) where

import           Universum

import qualified Data.ByteString                  as BS
import           Data.FileEmbed                   (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (stripPrefix)
import qualified Language.Haskell.TH              as TH
import           System.Directory                 (listDirectory)
import           System.FilePath                  (takeBaseName, takeExtension, (</>))
import           System.IO.Unsafe                 (unsafePerformIO)

import           Pos.Binary.Class                 (decodeFull)
import           Pos.Binary.Core.Genesis          ()
import           Pos.Binary.GodTossing            ()
import           Pos.Core.Constants               (genesisBinSuffix)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))
import           Pos.Util.Future                  (newInitFuture)

-- | Pre-generated genesis data from /genesis-godtossing.bin/ for all
-- genesis files (i.e. /genesis-godtossing-qa.bin/,
-- /genesis-godtossing-tns.bin/, etc). Each key in the map is a prefix
-- (“qa”, “tns”, etc).
allGenGtDatas :: HashMap String GenesisGtData
allGenGtDatas =
    let bins :: [(String, FilePath, ByteString)]
        bins = $(do
            dir <- makeRelativeToProject ""
            let getSuffix fp = do
                    guard (takeExtension fp == ".bin")
                    stripPrefix "genesis-godtossing-" (takeBaseName fp)
            suffs <- mapMaybe getSuffix <$> TH.runIO (listDirectory dir)
            let paths = suffs <&> \suff ->
                    dir </> ("genesis-godtossing-" <> suff <> ".bin")
            TH.listE $ zip suffs paths <&> \(suff, path) ->
                [|(suff, path, $(embedFile path))|]
            )
    in HM.fromList $ bins <&> \(suff, path, file) ->
           case decodeFull file of
               Left err -> error $ "Failed to read genesis from " <>
                                   toText path <> ": " <> err
               Right d
                   | null (ggdVssCertificates d) ->
                         error $ "No VSS certificates in " <> toText path
                   | otherwise -> (suff, d)

defaultGenGtData :: GenesisGtData
defaultGenGtData =
    case HM.lookup genesisBinSuffix allGenGtDatas of
        Just gd -> gd
        Nothing -> error $ "The config says that the genesis.bin suffix \
                           \should be " <> toText genesisBinSuffix <>
                           ", but corresponding file was not found in \
                           \'allGenGtDatas'"

genGtData :: GenesisGtData
setGenGtData :: GenesisGtData -> IO ()
(genGtData, setGenGtData) = unsafePerformIO newInitFuture
{-# NOINLINE genGtData #-}
{-# NOINLINE setGenGtData #-}

setGenGtDataFromFile :: FilePath -> IO ()
setGenGtDataFromFile path = do
    bs <- BS.readFile path
    case decodeFull bs of
        Left err -> error $ "Failed to read genesis from " <>
                            toText path <> ": " <> err
        Right d
            | null (ggdVssCertificates d) ->
                  error $ "No VSS certificates in " <> toText path
            | otherwise -> setGenGtData d
