module App where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

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
  position :: Field Point
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
        y: 3.0
      },
      time: 0.0
    }
  }
}

getComputedPosition :: State -> Number -> Point
getComputedPosition ({position: {value: v, func: f}}) time = 
  if timePassed > 0.0 
    then v
    else 
      applyPointValue v f.data time
  where
    timePassed = time - f.time

newPosition :: Point
newPosition = getComputedPosition initialState 20.0

data Shape
  = Circle Point Number
  | Rectangle Point Number Number

showPoint :: Point -> String
showPoint ({x: x, y: y}) =
  "(" <> show x <> ", " <> show y<> ")"

showShape :: Shape -> String
showShape (Circle c r) = "Circle with centre of " <> showPoint c <> ", and radius of " <> show r
showShape (Rectangle p w h) = "Reactangle at position " <> showPoint p <> " with width of " <> show w <> "and height of " <> show h

myCircle :: Shape
myCircle = Circle ({x: 0.0, y: 0.0}) 10.0

main :: forall a b. MonadEffect b => a -> b Unit
main time = do
  log $ showPoint newPosition