{-# OPTIONS_GHC -Wextra            #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Launcher.Logging
  (reportErrorDefault)
where

import           Universum
import           System.Directory                (createDirectoryIfMissing)
import           System.Environment              (getEnv)
import           System.FilePath                 ((</>))

getDefaultLogDir :: IO FilePath
getDefaultLogDir =
#ifdef mingw32_HOST_OS
    (</> "Daedalus\\Logs") <$> getEnv "APPDATA"
#elif defined (linux_HOST_OS)
    (</> "Daedalus/Logs") <$> getEnv "XDG_DATA_HOME"
#else
    (</> "Library/Application Support/Daedalus/Logs") <$> getEnv "HOME"
#endif

-- | Write @contents@ into @filename@ under default logging directory.
--
-- This function is only intended to be used before normal logging
-- is initialized. Its purpose is to provide at least some information
-- in cases where normal reporting methods don't work yet.
reportErrorDefault :: FilePath -> Text -> IO ()
reportErrorDefault filename contents = do
    logDir <- getDefaultLogDir
    createDirectoryIfMissing True logDir
    writeFile (logDir </> filename) contents
