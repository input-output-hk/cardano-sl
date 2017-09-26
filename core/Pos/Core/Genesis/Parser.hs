-- | Compile-time genesis data parser

module Pos.Core.Genesis.Parser
       ( allGenCoreDatas
       , defaultGenCoreData
       , genCoreData
       , setGenCoreData
       , setGenCoreDataFromFile
       ) where

import           Universum

import qualified Data.ByteString         as BS
import           Data.FileEmbed          (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (stripPrefix)
import qualified Language.Haskell.TH     as TH
import           System.Directory        (listDirectory)
import           System.FilePath         (takeBaseName, takeExtension, (</>))
import           System.IO.Unsafe        (unsafePerformIO)

import           Pos.Binary.Class        (decodeFull)
import           Pos.Binary.Core.Genesis ()
import           Pos.Core.Constants      (genesisBinSuffix)
import           Pos.Core.Genesis.Types  (GenesisCoreData (..))
import           Pos.Util.Future         (newInitFuture)

-- | Pre-generated genesis data from /genesis-core.bin/ for all genesis
-- files (i.e. /genesis-core-qa.bin/, /genesis-core-tns.bin/, etc). Each key
-- in the map is a prefix (“qa”, “tns”, etc).
allGenCoreDatas :: HashMap String GenesisCoreData
allGenCoreDatas =
    let bins :: [(String, FilePath, ByteString)]
        bins = $(do
            dir <- makeRelativeToProject ""
            let getSuffix fp = do
                    guard (takeExtension fp == ".bin")
                    stripPrefix "genesis-core-" (takeBaseName fp)
            suffs <- mapMaybe getSuffix <$> TH.runIO (listDirectory dir)
            let paths = suffs <&> \suff ->
                    dir </> ("genesis-core-" <> suff <> ".bin")
            TH.listE $ zip suffs paths <&> \(suff, path) ->
                [|(suff, path, $(embedFile path))|]
            )
    in HM.fromList $ bins <&> \(suff, path, file) ->
           case decodeFull file of
               Left err -> error $ "Failed to read genesis from " <>
                                   toText path <> ": " <> err
               Right d  -> (suff, d)

defaultGenCoreData :: GenesisCoreData
defaultGenCoreData =
    case HM.lookup genesisBinSuffix allGenCoreDatas of
        Just gd -> gd
        Nothing -> error $ "The config says that the genesis.bin suffix \
                           \should be " <> toText genesisBinSuffix <>
                           ", but corresponding file was not found in \
                           \'allGenCoreDatas'"

genCoreData :: GenesisCoreData
setGenCoreData :: GenesisCoreData -> IO ()
(genCoreData, setGenCoreData) =
    unsafePerformIO (newInitFuture "genCoreData")
{-# NOINLINE genCoreData #-}
{-# NOINLINE setGenCoreData #-}

setGenCoreDataFromFile :: FilePath -> IO ()
setGenCoreDataFromFile path = do
    bs <- BS.readFile path
    case decodeFull bs of
        Left err -> error $ "Failed to read genesis from " <>
                            toText path <> ": " <> err
        Right gd -> setGenCoreData gd
