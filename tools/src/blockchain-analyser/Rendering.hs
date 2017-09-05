
module Rendering ( render
                 , renderBlock
                 , renderBlocks
                 ) where

import qualified Data.Text          as T
import           Formatting         hiding (bytes)
import           Options            (CLIOptions (..), PrintMode (..), UOM (..))
import           Pos.Block.Core     (Block, BlockHeader, GenericBlockHeader, GenesisBlock,
                                     GenesisBlockHeader (..), MainBlock, MainBlockHeader,
                                     gbConsensus, gbhConsensus, getBlockHeader, _gbHeader,
                                     _gbhConsensus, _gcdEpoch)
import           Pos.Core           (EpochIndex, EpochOrSlot (..), HasCoreConstants,
                                     LocalSlotIndex (..), SlotId (..), getEpochIndex,
                                     getEpochOrSlot)
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

renderHeader :: CLIOptions -> Text
renderHeader cli = case printMode cli of
    Human      -> mempty
    AsciiTable ->
        let rows   = [["Block Type", "Epoch", "Slot", "Previous Block", "Block Hash"]]
            hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
            vdecor = DecorAll
            aligns = replicate 5 AlignLeft
        in tabl EnvAscii hdecor vdecor aligns rows
    CSV        -> "Block Type,Epoch,Slot,PrevBlock,BlockHash"

renderBlock :: HasCoreConstants
            => CLIOptions
            -> Block SscGodTossing
            -> Text
renderBlock cli block = case printMode cli of
    Human      -> renderBlockHuman block
    AsciiTable -> renderBlockTable [toTableRow block]
    CSV        -> renderBlockCSV block

renderBlockHuman :: HasCoreConstants => Block SscGodTossing -> Text
renderBlockHuman = either (sformat build) (sformat build)

renderBlockCSV :: HasCoreConstants => Block SscGodTossing -> Text
renderBlockCSV _   = "todo."

-- Epoch|Slot|Previous block|Block hash|Issuer|Number of txs|Size of header|Size of block (serialized)|Size of block+undo on disk
renderBlockTable :: [[Text]] -> Text
renderBlockTable rows =
  let hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
      vdecor = DecorAll
      aligns = replicate (length rows) AlignCentre
  in tabl EnvAscii hdecor vdecor aligns rows

renderBlocks :: HasCoreConstants
             => CLIOptions
             -> [Block SscGodTossing]
             -> Text
renderBlocks cli blocks = case printMode cli of
    Human      -> T.unlines $ map renderBlockHuman blocks
    AsciiTable ->
        let header = [
                  "Block Type"
                , "Epoch"
                , "Slot"
                , "Todo"
                ]
        in renderBlockTable (header : map toTableRow blocks)
    CSV        -> T.unlines $ map renderBlockCSV blocks


getEpoch :: BlockHeader SscGodTossing -> EpochIndex
getEpoch h = case unEpochOrSlot (getEpochOrSlot h) of
    Left e   -> e
    Right sl -> siEpoch sl

getSlot :: BlockHeader SscGodTossing -> Maybe SlotId
getSlot = either (const Nothing) Just . unEpochOrSlot . getEpochOrSlot

-- | Given a `Block`, returns a table row suitable for being printed
-- by `tabl`.
toTableRow :: HasCoreConstants => Block SscGodTossing -> [Text]
toTableRow block =
    let (blockHeader :: BlockHeader SscGodTossing) = getBlockHeader block
        epoch       = getEpochIndex (getEpoch blockHeader)
        blockType   = either (const "GENESIS") (const "MAIN") block
        slot        = maybe "-" (sformat build . getSlotIndex . siSlot) (getSlot blockHeader)
    in [blockType, sformat build epoch, sformat build slot, "todo" ] -- sformat build blockHeader]

{--
-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

data GenericBlock b = UnsafeGenericBlock
    { _gbHeader :: !(GenericBlockHeader b)
    , _gbBody   :: !(Body b)
    , _gbExtra  :: !(ExtraBodyData b)
    } deriving (Generic)
--}
