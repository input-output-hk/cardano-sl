-- | Left-align some text, that is, split it into words
-- and then eagerly allocate these words into lines, as
-- long as the total line length does not exceed the predefined
-- threshold. If a word is longer than the threshold, leave
-- it on a separate line.

module Pos.Util.Justify
       ( leftAlign
       ) where

import           Universum hiding ((<>))

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup ((<>))
import qualified Data.Text as Text

data Line = Line
  { lineWidth :: Int
  , lineWords :: NonEmpty Text
  }

initLine :: Text -> Line
initLine w = Line (Text.length w) (w :| [])

appendLine :: Line -> Line -> Line
appendLine line1 line2 =
  Line
    (lineWidth line1 + 1 + lineWidth line2)
    (lineWords line1 <>    lineWords line2)

nonEmptyTails :: NonEmpty a -> NonEmpty [a]
nonEmptyTails = NonEmpty.fromList . List.tail . List.tails . NonEmpty.toList

leftAlign :: Int -> Text -> [Text]
leftAlign desiredLineWidth =
    fmap mergeLine . groupWords . List.map initLine . words
  where
    groupWords :: [Line] -> [Line]
    groupWords = List.unfoldr (fmap @Maybe groupWords' . nonEmpty)

    groupWords' :: NonEmpty Line -> (Line, [Line])
    groupWords' ls =
        fromMaybe (NonEmpty.head groupings) $
            NonEmpty.last <$> nonEmpty goodGroupings
      where
        goodGroupings = NonEmpty.takeWhile (fits . fst) groupings
        groupings =
            NonEmpty.zip
                (NonEmpty.scanl1 appendLine ls)
                (nonEmptyTails ls)

    fits :: Line -> Bool
    fits line = lineWidth line <= desiredLineWidth

    mergeLine :: Line -> Text
    mergeLine = unwords . NonEmpty.toList . lineWords
