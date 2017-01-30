-- | Based on https://github.com/input-output-hk/vending-application/blob/master/web-client/src/Data/I18N.purs
module Explorer.I18n.Lang where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.String (take)
import Explorer.I18n.DE (translation) as DE
import Explorer.I18n.EN (translation) as EN

foreign import detectLocaleImpl :: forall e. Eff (dom :: DOM | e) String

detectLocale :: forall e. Eff (dom :: DOM | e) (Maybe Language)
detectLocale = readLanguage <<< take 2 <$> detectLocaleImpl

type I18nAccessor = (Translation -> String)

translate :: I18nAccessor -> Language -> String
translate f = f <<< getTranslation

-- | ISO 639-1 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
readLanguage :: String -> Maybe Language
readLanguage "en" = Just English
readLanguage "English" = Just English
readLanguage "de" = Just German
readLanguage "Deutsch" = Just German
readLanguage _ = Nothing

getTranslation :: Language -> Translation
getTranslation English = EN.translation
getTranslation German = DE.translation

data Language
    = English
    | German

instance showLanguage :: Show Language where
    show = languageNativeName

derive instance eqLanguage :: Eq Language

-- | ISO 639 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
languageNativeName :: Language -> String
languageNativeName English = "English"
languageNativeName German = "Deutsch"

type Translation =
    { title :: String
    , subtitle :: String
    , back :: String
    , transaction :: String
    , transactions :: String
    , transactionFeed :: String
    , address :: String
    , calculator :: String
    , version :: String
    , summary :: String
    , block :: String
    , hashes :: String
    , nav ::
      { home :: String
      , blockchain :: String
      , market :: String
      , charts :: String
      , tools :: String
    }
    , height :: String
    , age :: String
    , totalSent :: String
    , relayedBy :: String
    , sizeKB :: String
    }
