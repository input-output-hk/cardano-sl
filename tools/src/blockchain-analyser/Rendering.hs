
module Rendering ( render
                 , renderBlock
                 ) where

import qualified Data.Text          as T
import           Formatting         hiding (bytes)
import           Options            (PrintMode (..), UOM (..))
import           Pos.Block.Core     (Block, GenesisBlock, MainBlock)
import           Pos.Core           (HasCoreConstants)
import           Pos.Ssc.GodTossing (SscGodTossing)
import           Text.Tabl          (Alignment (..), Decoration (..),
                                     Environment (EnvAscii), tabl)
import           Types              (DBFolderStat)

import           Universum

-- Not using `Serokell.Data.Memory.Units` here, as it will automatically "promote" each
-- unit to the next multiplier. Example:
--
-- > sformat memory (fromBytes @Byte 1000)
-- "1000 B"
-- > sformat memory (fromBytes @Byte 1025)
-- "1.001 KiB"
--
-- What we want, instead, is to always convert from bytes to the requested unit of
-- measure, like other unix tools do.
renderBytes :: UOM -> Integer -> Text
renderBytes uom bytes =
    let formatPrecision = fixed @Double 3
        converted       = fromIntegral bytes / fromIntegral formatBytes
    in sformat formatPrecision converted
    where
      formatBytes :: Int
      formatBytes = case uom of
                        Bytes -> 1
                        KB    -> 1000
                        MB    -> 1000 * 1000
                        GB    -> 1000 * 1000 * 1000

renderUnit :: UOM -> Text
renderUnit uom = case uom of
    Bytes -> "B"
    KB    -> "KB"
    MB    -> "MB"
    GB    -> "GB"

renderBytesWithUnit :: UOM -> Integer -> Text
renderBytesWithUnit uom bytes = renderBytes uom bytes <> " " <> renderUnit uom

render :: UOM -> PrintMode -> [DBFolderStat] -> Text
render uom printMode stats =
    case printMode of
        CSV -> renderCSV uom stats
        _   -> renderAsciiTable uom stats

renderCSV :: UOM -> [DBFolderStat] -> Text
renderCSV uom stats =
    let header = ["Directory", "Size (" <> renderUnit uom <> ")"]
        rows   = header : map (\(f,sz) -> [f, renderBytes uom sz]) stats
    in T.unlines $ map (T.intercalate ",") rows

renderAsciiTable :: UOM -> [DBFolderStat] -> Text
renderAsciiTable uom stats =
    let rows = ["Directory", "Size"] : map (\(f,sz) -> [f, renderBytesWithUnit uom sz]) stats
    in tabl EnvAscii hdecor vdecor aligns rows
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    aligns = [AlignLeft, AlignLeft]

renderBlock :: HasCoreConstants
            => PrintMode
            -> Block SscGodTossing
            -> Text
renderBlock pMode b = case pMode of
    Human      -> either (sformat build) (sformat build) b
    AsciiTable -> renderBlockTable b
    CSV        -> renderBlockCSV b
    where
      renderBlockCSV _   = "todo."
      renderBlockTable _ = "todo."

{--
-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

data GenericBlock b = UnsafeGenericBlock
    { _gbHeader :: !(GenericBlockHeader b)
    , _gbBody   :: !(Body b)
    , _gbExtra  :: !(ExtraBodyData b)
    } deriving (Generic)
--}


-- (OldestFirst [] (Blund SscGodTossing))
