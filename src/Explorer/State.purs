module Explorer.State where

import Prelude
import Explorer.I18n.Lang (Language(English))


-- State

type State =
    { lang :: Language
    , count :: Int
    }

initialState :: State
initialState =
    { lang: English
    , count: 0
    }


-- Actions

data Action
    = Count
    | SetLanguage Language
    | NoOp


-- update

update :: Action -> State -> State
update Count state = state { count = state.count + 1 }
update (SetLanguage lang) state = state { lang = lang }
update NoOp state = state
