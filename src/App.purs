module App where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Math (abs)

data Tuple a b = Tuple a b

type Time = Number

type Point ={ 
  x :: Number, 
  y :: Number
}

type FieldFunc a = {
  data :: a,
  time :: Time
}

type Field a = { 
  value :: a, 
  func :: FieldFunc a
}

frameTime :: Number
frameTime = 1000.0 / 60.0

applyValue :: Number -> Number -> Time -> Number
applyValue value diff time =
  value + diff * (time / frameTime)

applyPointValue :: Point -> Point -> Time -> Point
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
        x: 20.0,
        y: 13.0
      },
      time: 0.0
    }
  },
  dummyField: 1.0
}

data Direction = N | E | W | S

data Action = Bounce Direction | Nothing

updatePointFieldFunc :: FieldFunc Point -> Time -> FieldFunc Point
updatePointFieldFunc f t =
  f {time = t}

updatePointField :: Field Point -> Point -> Time -> Field Point
updatePointField f d t =
  {func: (f.func {data = d, time = t}), value: (applyPointValue f.value f.func.data (t - f.func.time))}


setPositionData :: State -> Time -> (Point -> Point) -> State
setPositionData s t getData =
  s {position = updatePointField s.position (getData s.position.func.data) t}

update :: State -> Action -> Time -> State
update state action time = case action of
  Bounce N -> setPositionData state time (\d -> d {y = abs d.y})
  Bounce S -> setPositionData state time (\d -> d {y = -abs d.y})
  Bounce E -> setPositionData state time (\d -> d {x = -abs d.x})
  Bounce W -> setPositionData state time (\d -> d {x = abs d.x})
  _ -> state

getComputedPosition :: State -> Time -> Point
getComputedPosition ({position: {value: v, func: f}}) time = 
  if timePassed >= 0.0 
    then applyPointValue v f.data timePassed
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

getOutState :: State -> Time -> OutState
getOutState state time = 
    state { position = position }
  where
    position = getComputedPosition state time

main :: State -> Time -> Tuple State OutState
main state time = Tuple newState outState
  where
    outState = getOutState state time
    action = getAction outState
    newState = update state action time


bounceScreen :: Point -> Action
bounceScreen ({ x: x }) | x >= 128.0 = Bounce E
bounceScreen ({ x: x }) | x <= 0.0 = Bounce W
bounceScreen ({ y: y }) | y >= 128.0 = Bounce S
bounceScreen ({ y: y }) | y <= 0.0 = Bounce N
bounceScreen _ = Nothing


getAction :: OutState -> Action
getAction state = bounceScreen state.position