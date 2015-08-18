module App where

import Mouse
import Window

import Graphics.Element exposing (show)
import Graphics.Collage exposing (collage, segment, solid, traced, path, toForm)

import Color exposing (red)

import Random exposing (generate, initialSeed, int)


mouseMailbox : Signal.Mailbox Action
mouseMailbox = Signal.mailbox Nothing

type alias Model = {
    points : List (Float, Float),
    windowSize : (Float, Float)
}

type Action = MouseMove (Float, Float) | WindowResize (Float, Float) | Nothing

model : Model
model = {
  points = [],
  windowSize = (500, 500) }

drawPoints model = 
  traced (solid red) 
  <| path <| model.points

view address model = 
  collage (round <| fst model.windowSize) (round <| snd model.windowSize) [
    drawPoints model,
    toForm <| show model
  ]

update : Action -> Model -> Model 
update action model =
  case action of 
    MouseMove (x, y) ->
      let
        winX = fst model.windowSize
        winY = snd model.windowSize
        newX = x - (winX / 2)
        newY = ((winY / 2)) - y
      in 
        { model | points <- (newX, newY) :: model.points }
    WindowResize (x, y) -> { model | windowSize <- (x, y)}
    Nothing -> model

model' =
  Signal.foldp
    update
    model
    <|
      Signal.merge
        (Signal.map (\(x, y) -> WindowResize (toFloat x, toFloat y)) Window.dimensions)
        (Signal.map2 (\isDown (x, y) -> if isDown then MouseMove (toFloat x, toFloat y) else Nothing) Mouse.isDown Mouse.position )


main = Signal.map (view mouseMailbox.address) model'