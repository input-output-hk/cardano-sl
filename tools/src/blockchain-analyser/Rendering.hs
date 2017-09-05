
module Rendering ( render
                 , renderBlock
                 , renderBlocks
                 , renderHeader
                 ) where

import qualified Data.Text          as T
import           Formatting         hiding (bytes)
import           Options            (CLIOptions (..), PrintMode (..), UOM (..))
import           Pos.Block.Core     (Block, BlockHeader, GenericBlockHeader, GenesisBlock,
                                     GenesisBlockHeader (..), MainBlock, MainBlockHeader,
                                     blockHeaderHash, gbConsensus, gbhConsensus,
                                     getBlockHeader, _gbHeader, _gbhConsensus, _gcdEpoch,
                                     _mcdLeaderKey)
import           Pos.Core           (EpochIndex, EpochOrSlot (..), HasCoreConstants,
                                     LocalSlotIndex (..), SlotId (..), getEpochIndex,
                                     getEpochOrSlot)
import           Pos.Crypto         (PublicKey)
import           Pos.Ssc.GodTossing (SscGodTossing)
import           Text.Tabl          (Alignment (..), Decoration (..),
                                     Environment (EnvAscii), tabl)
import           Types              (DBFolderStat, prevBlock)

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
            => CLIOptions
            -> Block SscGodTossing
            -> Text
renderBlock cli block = case printMode cli of
    Human      -> renderBlockHuman block
    AsciiTable -> let rows = [toTableRow block]
                  in renderAsTable DecorNone DecorNone (defaultAlignment rows) rows
    CSV        -> renderBlockCSV block

renderBlockHuman :: HasCoreConstants => Block SscGodTossing -> Text
renderBlockHuman = either (sformat build) (sformat build)

renderBlockCSV :: HasCoreConstants => Block SscGodTossing -> Text
renderBlockCSV = T.intercalate "," . toTableRow

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
    AsciiTable -> renderAsTable defaultHorizontalDecoration defaultVerticalDecoration (defaultAlignment [header]) [header]
    CSV        -> T.intercalate "," header

header :: [T.Text]
header = [
           "Block Type"
         , "Epoch"
         , "Slot"
         , "Previous Block"
         , "Block Hash"
         , "Leader"
         ]

renderBlocks :: HasCoreConstants
             => CLIOptions
             -> [Block SscGodTossing]
             -> Text
renderBlocks cli blocks = case printMode cli of
    Human      -> T.unlines $ map renderBlockHuman blocks
    AsciiTable -> let rows = header : map toTableRow blocks
                  in renderAsTable defaultHorizontalDecoration defaultVerticalDecoration (defaultAlignment rows) rows
    CSV        -> T.unlines (renderHeader cli : map renderBlockCSV blocks)


getEpoch :: BlockHeader SscGodTossing -> EpochIndex
getEpoch h = case unEpochOrSlot (getEpochOrSlot h) of
    Left e   -> e
    Right sl -> siEpoch sl

getSlot :: BlockHeader SscGodTossing -> Maybe SlotId
getSlot = either (const Nothing) Just . unEpochOrSlot . getEpochOrSlot

getLeader :: BlockHeader SscGodTossing -> Maybe PublicKey
getLeader (Left _)   = Nothing
getLeader (Right bh) = Just . _mcdLeaderKey . _gbhConsensus $ bh

-- | Given a `Block`, returns a table row suitable for being printed
-- by `tabl`.
toTableRow :: HasCoreConstants => Block SscGodTossing -> [Text]
toTableRow block =
    let blockHeader   = getBlockHeader block
        previousBlock = sformat build (prevBlock block)
        blockHash     = sformat build (blockHeaderHash blockHeader)
        epoch         = sformat build (getEpochIndex (getEpoch blockHeader))
        blockType     = either (const "GENESIS") (const "MAIN") block
        slot          = maybe mempty (sformat build . getSlotIndex . siSlot) (getSlot blockHeader)
        leader        = maybe mempty (sformat build) (getLeader blockHeader)
    in [blockType, epoch, slot, previousBlock, blockHash, leader]

{--
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

MainBlockHeader:\n
    hash: de3d754d0895aa1d550578a0604d480dfa92b9bef08b07ba74c42dd0661f8bea\n
    previous block: b47759b8742e4064c1049375a1b99c39e1aad7f688450b650eb82931dc81b621\n
    slot: 0th slot of 0th epoch\n
    difficulty: 1\n
    leader: pub:80b70572\n
    signature: BlockSignature: <signature>\n
    block: v0.0.0\n
    software: csl-daedalus:0\n
GenesisBlockHeader:\n
    hash: b47759b8742e4064c1049375a1b99c39e1aad7f688450b650eb82931dc81b621\n
    previous block: 791f4256e14c67b9035c3b80a0826adf719d3636c18eef16c98b84b833723d51\n
    epoch: epoch #0\n
    difficulty: 0\n
--}
