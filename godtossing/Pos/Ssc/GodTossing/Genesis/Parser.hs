{-# LANGUAGE CPP #-}

module Pos.Ssc.GodTossing.Genesis.Parser
       ( compileGenGtData
       ) where

import           Universum

#ifdef NO_EMBED
import qualified Data.ByteString                  as BS
import           System.Environment               (getEnv)
import           System.FilePath                  ((</>))
import           System.IO.Unsafe                 (unsafePerformIO)
#else
import           Data.FileEmbed                   (embedFile, makeRelativeToProject)
#endif

import           Pos.Binary.Class                 (decodeFull)
import           Pos.Binary.GodTossing.Types      ()
import           Pos.Core.Constants               (genesisBinSuffix)
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))

-- | Fetch pre-generated genesis data from /genesis-godtossing.bin/ in
-- compile time.
--
-- Note that if the @no-embed@ flag is set, genesis will be read at runtime
-- from the folder determined by the @CSL_RES_PATH@ environment variable.
compileGenGtData :: GenesisGtData
compileGenGtData = do
    let name = "genesis-godtossing-" <> genesisBinSuffix <> ".bin"
#ifdef NO_EMBED
    let path = unsafePerformIO $ getEnv "CSL_RES_PATH"
    let file = unsafePerformIO $ BS.readFile (path </> name)
#else
    let file = $(embedFile =<< makeRelativeToProject name)
#endif
    case decodeFull file of
        Left a  -> error $ toText a
        Right d -> if null (ggdVssCertificates d)
                   then error "No VSS certificates in genesis-godtossing.bin"
                   else d
