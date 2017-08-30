{-# LANGUAGE CPP #-}

-- | This is a separate module due to the TH stage restriction
module Pos.Util.Config.Path
       ( cslConfigPath
       ) where

import           Universum                  hiding (lift)

#ifdef NO_EMBED
import           System.Environment         (getEnv)
import           System.FilePath            ((</>))
import           System.IO.Unsafe           (unsafePerformIO)
#else
import           Language.Haskell.TH.Syntax (lift, loc_filename, qLocation, runIO)
import           System.Directory           (canonicalizePath, getDirectoryContents)
import           System.FilePath            (takeDirectory, takeFileName, (</>))
#endif

-- | Path to config that should be used by all parts of Cardano SL.
--
-- The name of the config is @constants.yaml@ and by default we try to find
-- it in source dir at compile time, but if the @no-embed@ flag is set, we
-- will look for it in the directory specified by the @CSL_RES_PATH@
-- environment variable.

#ifdef NO_EMBED

cslConfigPath :: FilePath
cslConfigPath = unsafePerformIO $
    getEnv "CSL_RES_PATH" <&> (</> "constants.yaml")
{-# NOINLINE cslConfigPath #-}

#else

cslConfigPath :: FilePath
cslConfigPath = $(do
    let name = "constants.yaml"
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
    lift path
  )

#endif
