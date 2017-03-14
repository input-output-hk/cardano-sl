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

    -- This code was stolen from file-embed ('makeRelativeToProject'). We
    -- don't use file-embed because the config-finding logic has already been
    -- changed several times and switching from file-embed to custom logic
    -- and back is annoying.
    let marker = "cardano-sl-core.cabal"
        findConfigDir x = do
            let dir = takeDirectory x
            contents <- getDirectoryContents dir
            let isRoot = any ((== marker) . takeFileName) contents
            if | dir == x  -> return Nothing
               | isRoot    -> return (Just dir)
               | otherwise -> findConfigDir dir
    loc <- qLocation
    path <- runIO $ do
        srcFP <- canonicalizePath $ loc_filename loc
        mdir <- findConfigDir srcFP
        case mdir of
            Just dir -> return (dir </> name)
            Nothing  -> error $ toText $
                "Could not find " ++ marker ++ " for path: " ++ srcFP
    lift path)
