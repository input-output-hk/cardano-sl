module Explorer.State where

import Prelude


-- State

type State = {
    count :: Int
}

initialState :: State
initialState = {
    count: 0
}


-- Actions

data Action
    = Count
    | NoOp


-- update

update :: Action -> State -> State
update Count state = state { count = state.count + 1 }
update NoOp state = state
