module Explorer.Util.String
    ( substitute
    , parseSearchEpoch
    , formatADA
    ) where

import BigNumber (BigNumberFormat(..), defaultFormat, dividedByInt, toFormat, fromString) as BN
import Control.Alt ((<|>))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (many)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cDecimalSeparator, cGroupSeparator) as I18nL
import Explorer.Types.State (SearchEpochSlotQuery)
import Pos.Explorer.Web.ClientTypes (CCoin)
import Pos.Explorer.Web.Lenses.ClientTypes (getCoin, _CCoin)
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
    _ <- char ','
    slot  <- many digit >>= pure <<< fromString <<< fromCharArray
    pure $ Tuple epoch slot

-- | Combine both parsers
parseEpochOrEpochSlot :: Parser String SearchEpochSlotQuery
parseEpochOrEpochSlot = try parseSearchEpochSlotQuery <|> parseSearchEpochQuery

-- | Run the actual parser
parseSearchEpoch :: String -> Either ParseError SearchEpochSlotQuery
parseSearchEpoch input = runParser input parseEpochOrEpochSlot

formatADA :: CCoin -> Language -> String
formatADA coin lang =
    case BN.fromString $ coin ^. (_CCoin <<< getCoin) of
        Nothing -> ""
        Just bigNumber -> do
            let bigNumberADA = BN.dividedByInt bigNumber lovelacesADA
            unsafePerformEff $ BN.toFormat bigNumberADA newFormat decimalPlacesADA
    where
        decimalPlacesADA = 6
        lovelacesADA = 1000000
        newFormat = BN.BigNumberFormat $ (unwrap BN.defaultFormat)
                        { decimalSeparator = translate (I18nL.common <<< I18nL.cDecimalSeparator) lang
                        , groupSeparator = translate (I18nL.common <<< I18nL.cGroupSeparator) lang
                        }
