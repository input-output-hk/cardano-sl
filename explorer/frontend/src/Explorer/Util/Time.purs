module Explorer.Util.Time (
  prettyDuration
  , nominalDiffTimeToDateTime
  , prettyDate
  ) where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String (joinWith, trim)
import Data.Time.Duration (class Duration, Days(..), Hours(..), Minutes(..), convertDuration, fromDuration)
import Data.Time.NominalDiffTime (NominalDiffTime(..))
import Data.Tuple (uncurry, Tuple(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cDays, cHours, cMinutes) as I18nL

-- show readable duration
prettyDuration :: forall a. Duration a => Language -> a -> String
prettyDuration lang dur | convertDuration dur < Minutes 1.0 = "< 1 " <> translate (I18nL.common <<< I18nL.cMinutes) lang
                        | otherwise = trim $ joinWith " " $ map (uncurry showIfNonZero)
                             [ Tuple d translationDays
                             , Tuple h translationHours
                             , Tuple m translationMinutes
                             ]
  where
    translationMinutes  = translate (I18nL.common <<< I18nL.cMinutes) lang
    translationHours    = translate (I18nL.common <<< I18nL.cHours) lang
    translationDays     = translate (I18nL.common <<< I18nL.cDays) lang
    m = floor $ un Minutes (convertDuration dur `sub` convertDuration (Days $ toNumber d) `sub` convertDuration (Hours $ toNumber h))
    h = floor $ un Hours (convertDuration dur `sub` convertDuration (Days $ toNumber d))
    d = floor $ un Days (convertDuration dur)
    showIfNonZero nu st =
        if nu == 0
            then ""
            else show nu <> " " <> st

nominalDiffTimeToDateTime :: NominalDiffTime -> Maybe DateTime
nominalDiffTimeToDateTime (NominalDiffTime s) =
    map toDateTime <<< instant $ fromDuration s


type TimeFormat = String

prettyDate' :: TimeFormat -> DateTime -> Maybe String
prettyDate' format dateTime =
  either (const Nothing) Just $ formatDateTime format dateTime

prettyDate :: TimeFormat -> NominalDiffTime -> Maybe String
prettyDate format time = do
    dateTime <- nominalDiffTimeToDateTime time
    pretty <- prettyDate' format dateTime
    pure pretty
