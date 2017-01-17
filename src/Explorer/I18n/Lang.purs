-- | Based on https://github.com/input-output-hk/vending-application/blob/master/web-client/src/Data/I18N.purs
module Explorer.I18n.Lang where

import Prelude
import DOM (DOM)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe (..))
import Data.String (take)
import Explorer.I18n.DE (translation) as DE
import Explorer.I18n.EN (translation) as EN

foreign import detectLocaleImpl :: forall e. Eff (dom :: DOM | e) String

detectLocale :: forall e. Eff (dom :: DOM | e) (Maybe Language)
detectLocale = readLanguage <<< take 2 <$> detectLocaleImpl

translate :: (Translation -> String) -> Language -> String
translate f = f <<< getTranslation

-- | ISO 639-1 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
readLanguage :: String -> Maybe Language
readLanguage "en" = Just English
readLanguage "de" = Just German
readLanguage _ = Nothing

getTranslation :: Language -> Translation
getTranslation English = EN.translation
getTranslation German = DE.translation

data Language
    = English
    | German

instance showLanguage :: Show Language where
    show = languageNativeName

-- | ISO 639 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
languageNativeName :: Language -> String
languageNativeName English = "English"
languageNativeName German = "Deutsch"

type Translation =
    { welcome :: String
    , count :: String
    , counted :: String
    , hello :: String
    }
