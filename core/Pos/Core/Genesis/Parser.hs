{-# LANGUAGE CPP #-}

-- | Compile-time genesis data parser

module Pos.Core.Genesis.Parser
       ( compileGenCoreData
       ) where

import           Universum               hiding (lift)

#ifdef NO_EMBED
import qualified Data.ByteString         as BS
import           System.Environment      (getEnv)
import           System.FilePath         ((</>))
import           System.IO.Unsafe        (unsafePerformIO)
#else
import           Data.FileEmbed          (embedFile, makeRelativeToProject)
#endif

import           Pos.Binary.Class        (decodeFull)
import           Pos.Binary.Core.Genesis ()
import           Pos.Core.Constants      (genesisBinSuffix)
import           Pos.Core.Genesis.Types  (GenesisCoreData (..))

-- | Fetch pre-generated genesis data from /genesis-core.bin/ in compile
-- time. Doesn't use TH with lift because it's difficult to provide 'Lift'
-- instance to 'GenesisCoreData'
--
-- Note that if the @no-embed@ flag is set, genesis will be read at runtime
-- from the folder determined by the @CSL_RES_PATH@ environment variable.
compileGenCoreData :: GenesisCoreData
compileGenCoreData = do
    let name = "genesis-core-" <> genesisBinSuffix <> ".bin"
#ifdef NO_EMBED
    let path = unsafePerformIO $ getEnv "CSL_RES_PATH"
    let file = unsafePerformIO $ BS.readFile (path </> name)
#else
    let file = $(embedFile =<< makeRelativeToProject name)
#endif
    case decodeFull file of
        Left a  -> error $ "Failed to read genesis: " <> toText a
        Right d -> d
