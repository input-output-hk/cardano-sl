
module Rendering ( render
                 , renderBlock
                 , renderBlocks
                 , renderHeader
                 ) where

import qualified Data.Text                  as T
import           Formatting                 hiding (bytes)
import           Options                    (CLIOptions (..), PrintMode (..), UOM (..))
import           Pos.Binary.Class           (biSize)
import           Pos.Block.Core             (Block, BlockHeader, blockHeaderHash,
                                             getBlockHeader, mbTxs, _gbBody,
                                             _gbhConsensus, _mcdLeaderKey)
import           Pos.Block.Types            (Undo)
import           Pos.Core                   (EpochIndex, EpochOrSlot (..),
                                             HasCoreConstants, LocalSlotIndex (..),
                                             SlotId (..), getEpochIndex, getEpochOrSlot)
import           Pos.Crypto                 (PublicKey)
import           Pos.Merkle                 (MerkleTree (..))
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Txp.Core               (Tx)
import           Serokell.Data.Memory.Units (toBytes)
import           Text.Tabl                  (Alignment (..), Decoration (..),
                                             Environment (EnvAscii), tabl)
import           Types                      (DBFolderStat, prevBlock)

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
    let statsHeaders = ["Directory", "Size (" <> renderUnit uom <> ")"]
        rows   = statsHeaders : map (\(f,sz) -> [f, renderBytes uom sz]) stats
    in unlines $ map (T.intercalate ",") rows

renderAsciiTable :: UOM -> [DBFolderStat] -> Text
renderAsciiTable uom stats =
    let rows = ["Directory", "Size"] : map (\(f,sz) -> [f, renderBytesWithUnit uom sz]) stats
    in tabl EnvAscii hdecor vdecor aligns rows
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    aligns = [AlignLeft, AlignLeft]

renderBlock :: HasCoreConstants
            => CLIOptions
            -> (Block SscGodTossing, Maybe Undo)
            -> Text
renderBlock cli block = case printMode cli of
    Human      -> renderBlockHuman (fst block)
    AsciiTable -> let rows = [toTableRow (uom cli) block]
                  in renderAsTable DecorNone DecorNone (defaultAlignment rows) rows
    CSV        -> renderBlockCSV (uom cli) block

renderBlockHuman :: HasCoreConstants => Block SscGodTossing -> Text
renderBlockHuman = either (sformat build) (sformat build)

renderBlockCSV :: HasCoreConstants => UOM -> (Block SscGodTossing, Maybe Undo) -> Text
renderBlockCSV uom = T.intercalate "," . (toTableRow uom)

defaultHorizontalDecoration :: Decoration
defaultHorizontalDecoration = DecorUnion [DecorOuter, DecorOnly [1]]

defaultVerticalDecoration :: Decoration
defaultVerticalDecoration = DecorAll

defaultAlignment :: [a] -> [Alignment]
defaultAlignment rows = replicate (length rows) AlignCentre

renderAsTable :: Decoration
              -> Decoration
              -> [Alignment]
              -> [[Text]] -> Text
renderAsTable hdecor vdecor aligns rows = tabl EnvAscii hdecor vdecor aligns rows

renderHeader :: CLIOptions -> Text
renderHeader cli = case printMode cli of
    Human      -> mempty
    AsciiTable -> let rows = [ header (uom cli) ]
                  in renderAsTable defaultHorizontalDecoration defaultVerticalDecoration (defaultAlignment rows) rows
    CSV        -> T.intercalate "," (header (uom cli))

header :: UOM -> [T.Text]
header uom = [
           "Block Type"
         , "Epoch"
         , "Slot"
         , "Previous Block"
         , "Block Hash"
         , "Leader"
         , "Tx Count"
         , "Header Size  (" <> renderUnit uom <> ")"
         , "Block  Size  (" <> renderUnit uom <> ")"
         , "Block + Undo (" <> renderUnit uom <> ")"
         ]

renderBlocks :: HasCoreConstants
             => CLIOptions
             -> [(Block SscGodTossing, Maybe Undo)]
             -> Text
renderBlocks cli blocks = case printMode cli of
    Human      -> unlines $ map (renderBlockHuman . fst) blocks
    AsciiTable -> let rows = header (uom cli) : map (toTableRow (uom cli)) blocks
                  in renderAsTable defaultHorizontalDecoration defaultVerticalDecoration (defaultAlignment rows) rows
    CSV        -> unlines (renderHeader cli : map (renderBlockCSV (uom cli)) blocks)


getEpoch :: BlockHeader SscGodTossing -> EpochIndex
getEpoch h = case unEpochOrSlot (getEpochOrSlot h) of
    Left e   -> e
    Right sl -> siEpoch sl

getSlot :: BlockHeader SscGodTossing -> Maybe SlotId
getSlot = either (const Nothing) Just . unEpochOrSlot . getEpochOrSlot

getLeader :: BlockHeader SscGodTossing -> Maybe PublicKey
getLeader (Left _)   = Nothing
getLeader (Right bh) = Just . _mcdLeaderKey . _gbhConsensus $ bh

getTxs :: Block SscGodTossing -> MerkleTree Tx
getTxs (Left _)          = MerkleEmpty
getTxs (Right mainBlock) = (_gbBody mainBlock) ^. mbTxs

getHeaderSize :: HasCoreConstants => BlockHeader SscGodTossing -> Integer
getHeaderSize = either (toBytes . biSize) (toBytes . biSize)

getBlockSize :: HasCoreConstants => Block SscGodTossing -> Integer
getBlockSize = either (toBytes . biSize) (toBytes . biSize)

getUndoSize :: HasCoreConstants => Maybe Undo -> Integer
getUndoSize = maybe 0 (toBytes . biSize)

-- | Given a `Block`, returns a table row suitable for being printed
-- by `tabl`.
toTableRow :: HasCoreConstants => UOM -> (Block SscGodTossing, Maybe Undo) -> [Text]
toTableRow uom (block, mbUndo) =
    let blockHeader   = getBlockHeader block
        previousBlock = sformat build (prevBlock block)
        blockHash     = blockHeaderHash blockHeader
        epoch         = sformat build (getEpochIndex (getEpoch blockHeader))
        blockType     = either (const "GENESIS") (const "MAIN") block
        slot          = maybe mempty (sformat build . getSlotIndex . siSlot) (getSlot blockHeader)
        leader        = maybe mempty (sformat build) (getLeader blockHeader)
        txCount       = sformat build (length (getTxs block))
        headerSize    = renderBytesWithUnit uom (getHeaderSize blockHeader)
        blockSize     = getBlockSize block
        undoSize      = getUndoSize  mbUndo
    in [ blockType
       , epoch
       , slot
       , previousBlock
       , sformat build blockHash
       , leader
       , txCount
       , headerSize
       , renderBytesWithUnit uom blockSize
       , renderBytesWithUnit uom (blockSize + undoSize)
       ]
