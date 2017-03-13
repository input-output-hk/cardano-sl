module Explorer.Util.Time (prettyDuration) where

import Prelude

import Data.Int (floor, toNumber)
import Data.String (trim, joinWith)
import Data.Tuple (uncurry, Tuple(..))
import Data.Newtype (unwrap, un)
import Data.Time.Duration (class Duration, Milliseconds(..), Minutes(..), Hours(..), Days(..), convertDuration, toDuration, fromDuration)
import Explorer.I18n.Lenses (common, cDays, cHours, cMinutes, cSeconds) as I18nL
import Explorer.I18n.Lang (Language, languageNativeName, translate)

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
