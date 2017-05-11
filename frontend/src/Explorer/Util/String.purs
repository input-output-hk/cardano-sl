module Explorer.Util.String (substitute, parseSearchEpoch, take) where

import Control.Alt ((<|>))
import Data.Array (many)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Explorer.Types.State (SearchEpochSlotQuery)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (digit)
import Prelude hiding (between,when)

foreign import substituteImpl :: String -> Array String -> String

-- | Substitutes `{0}` placeholders of a string
-- | * `substitute "Hello {0}, what's going on {1}" ["Jane", "today"]`
-- | * `-- output: "Hello Jane, what's going on today"`
substitute :: String -> Array String -> String
substitute = substituteImpl

-- | A simple parser for the epoch:
-- | ```purescript
-- | parseSearchEpochSlotQuery "256"
-- | ```
parseSearchEpochQuery :: Parser String SearchEpochSlotQuery
parseSearchEpochQuery = do
    epoch <- many digit >>= pure <<< fromString <<< fromCharArray
    pure $ Tuple epoch Nothing

-- | A simple parser for the epoch and slot:
-- | ```purescript
-- | parseSearchEpochSlotQuery "256,12"
-- | ```
parseSearchEpochSlotQuery :: Parser String SearchEpochSlotQuery
parseSearchEpochSlotQuery = do
    epoch <- many digit >>= pure <<< fromString <<< fromCharArray
    char ','
    slot  <- many digit >>= pure <<< fromString <<< fromCharArray
    pure $ Tuple epoch slot

-- | Combine both parsers
parseEpochOrEpochSlot :: Parser String SearchEpochSlotQuery
parseEpochOrEpochSlot = try parseSearchEpochSlotQuery <|> parseSearchEpochQuery

-- | Run the actual parser
parseSearchEpoch :: String -> Either ParseError SearchEpochSlotQuery
parseSearchEpoch input = runParser input parseEpochOrEpochSlot

-- | Returns the first `n` characters of the string.
foreign import take :: Int -> String -> String
