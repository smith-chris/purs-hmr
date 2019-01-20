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
        x: 15.0,
        y: 2.0
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


-- let getNewPointField (field: Field<Point>) (data: Point) (time: float) : Field<Point> =
--   {field with func = {data = data; time = time}; value = (applyPointValue field.value field.func.data (time - field.func.time))}

setPositionData :: State -> Time -> (Point -> Point) -> State
setPositionData s t getData =
  s {position = updatePointField s.position (getData s.position.func.data) t}

update :: State -> Action -> Time -> State
update state action time = case action of
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

getAction :: OutState -> Action
getAction state = 
  if state.position.x >= 128.0
    then Bounce E
  else if state.position.x <= 0.0
    then Bounce W
  else 
    Nothing