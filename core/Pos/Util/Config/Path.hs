{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This is a separate module due to the TH stage restriction
module Pos.Util.Config.Path
       ( cslConfigFilePath
       ) where

import           Language.Haskell.TH.Syntax (lift, loc_filename, qLocation, runIO)
import           System.Directory           (canonicalizePath, getDirectoryContents)
import           System.FilePath            (takeDirectory, takeFileName, (</>))
import           Universum                  hiding (lift)

-- | Path to config that should be used by all parts of Cardano SL.
--
-- The name of the config is
--
--   * "constants-dev.yaml" in development mode
--   * "constants-prod.yaml" in production mode
--   * "constants-wallet-prod.yaml" in production mode with wallet
--
-- TODO: allow overriding the config path via an env var?
cslConfigFilePath :: FilePath
cslConfigFilePath = $(do

#if defined(DEV_MODE)
    let name = "constants-dev.yaml"
#elif defined(WITH_WALLET)
    let name = "constants-wallet-prod.yaml"
#else
    let name = "constants-prod.yaml"
#endif

    -- This code was stolen from file-embed ('makeRelativeToProject').
    -- Unfortunately, we can't use it directly because we have cabal files in
    -- subfolders as well, but we need to stop at cardano-sl.cabal
    -- specifically and not at e.g. cardano-sl-core.cabal.
    let findProjectDir x = do
            let dir = takeDirectory x
            if dir == x
                then return Nothing
                else do
                    contents <- getDirectoryContents dir
                    if any ((== "cardano-sl.cabal") . takeFileName) contents
                        then return (Just dir)
                        else findProjectDir dir
    loc <- qLocation
    path <- runIO $ do
        srcFP <- canonicalizePath $ loc_filename loc
        mdir <- findProjectDir srcFP
        case mdir of
            Just dir -> return (dir </> name)
            Nothing  -> error $ toText $
                "Could not find cardano-sl.cabal for path: " ++ srcFP

    lift path)
