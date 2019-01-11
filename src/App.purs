module App where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

data Shape
  = Circle Point Number
  | Rectangle Point Number Number

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point {x: x, y: y}) =
  "(" <> show x <> ", " <> show y<> ")"

showShape :: Shape -> String
showShape (Circle c r) = "Circle with centre of " <> showPoint c <> ", and radius of " <> show r
showShape (Rectangle p w h) = "Reactangle at position " <> showPoint p <> " with width of " <> show w <> "and height of " <> show h

myCircle :: Shape
myCircle = Circle (Point {x: 0.0, y: 0.0} ) 10.0

main :: forall a b. MonadEffect b => a -> b Unit
main time = do
  log $ showShape myCircle