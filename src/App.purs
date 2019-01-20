module App where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

data Tuple a b = Tuple a b

type Point ={ 
  x :: Number, 
  y :: Number
}

type FieldFunc a = {
  data :: a,
  time :: Number
}

type Field a = { 
  value :: a, 
  func :: FieldFunc a
}

frameTime :: Number
frameTime = 1000.0 / 60.0

applyValue :: Number -> Number -> Number -> Number
applyValue value diff time =
  value + diff * (time /frameTime)

applyPointValue :: Point -> Point -> Number -> Point
applyPointValue v d t = {x: x, y: y}
  where
    x = applyValue v.x d.x t
    y = applyValue v.y d.y t

type State = {
  position :: Field Point,
  dummyField :: Number
}

type OutState = {
  position :: Point,
  dummyField :: Number
}

initialState :: State
initialState = {
  position: {
    value: {
      x: 0.0,
      y: 0.0
    },
    func: {
      data: {
        x: 15.0,
        y: 3.0
      },
      time: 0.0
    }
  },
  dummyField: 1.0
}

data Direction = N | E | W | S

data Action = Bounce Direction | Nothing

update :: State -> Action -> State
update state action = case action of
  Bounce S -> state {dummyField = 5.0}
  _ -> state

getComputedPosition :: State -> Number -> Point
getComputedPosition ({position: {value: v, func: f}}) time = 
  if timePassed >= 0.0 
    then applyPointValue v f.data time
    else 
      v
  where
    timePassed = time - f.time

showPoint :: Point -> String
showPoint ({x: x, y: y}) =
  "(" <> show x <> ", " <> show y<> ")"

data Output = Output OutState Action

errorPoint :: Point
errorPoint = {x: -1.0, y: -1.0}

getOutState :: State -> Number -> OutState
getOutState state time = 
    state { position = position }
  where
    position = getComputedPosition state time

main :: State -> Number -> Tuple State OutState
main state time = Tuple newState outState
  where
    outState = getOutState state time
    action = getAction outState
    newState = update state action

getAction :: OutState -> Action
getAction state =
  if state.position.x >= 50.0
    then Bounce S
  else 
    Nothing