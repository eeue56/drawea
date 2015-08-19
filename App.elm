module App where

import Mouse
import Keyboard
import Window

import Graphics.Element exposing (show)
import Graphics.Collage exposing (..)

import Color exposing (Color, red, blue, yellow, black)
import Set exposing (Set)

import Random exposing (generate, initialSeed, int)


mouseMailbox : Signal.Mailbox Action
mouseMailbox = Signal.mailbox Nothing

type alias Model = {
  color : Color,
  points : List (Float, Float),
  mouseAt : (Float, Float),
  windowSize : (Float, Float)
}

type Action = 
  MouseClick (Float, Float) | 
  MouseMove (Float, Float) | 
  WindowResize (Float, Float) | 
  KeyDown (Set Keyboard.KeyCode) |
  Nothing

model : Model
model = {
  color = red,
  points = [],
  mouseAt = (50, 50),
  windowSize = (3000, 3000) }

drawPoints model = 
  traced (solid model.color) 
    <| path model.points

drawBlotch color = 
  rect 50 50
    |> filled color

drawPointer model = 
  oval 5 5
    |> filled model.color
    |> move model.mouseAt

view address model = 
  collage (round <| fst model.windowSize) (round <| snd model.windowSize) [
    drawPoints model,
    drawPointer model
  ]

scaledValues x y model =
  let 
    winX = fst model.windowSize
    winY = snd model.windowSize
    newX = x - (winX / 2)
    newY = ((winY / 2)) - y
  in
    (newX, newY)

swapColor keys = if
  | Set.member 49 keys -> red 
  | Set.member 50 keys -> blue 
  | Set.member 51 keys -> yellow 
  | otherwise -> black 

update : Action -> Model -> Model 
update action model =
  case action of 
    MouseClick (x, y) ->
      let
        (newX, newY) = scaledValues x y model
      in 
        { model | points <- (newX, newY) :: model.points }
    MouseMove (x, y) -> { model | mouseAt <- scaledValues x y model }
    WindowResize (x, y) -> { model | windowSize <- (x, y)}
    KeyDown keys -> if not <| Set.isEmpty keys then { model | color <- swapColor keys } else model
    Nothing -> model

model' =
  Signal.foldp
    update
    model
    <|
      Signal.mergeMany
        [ Signal.map (\(x, y) -> WindowResize (toFloat x, toFloat y)) Window.dimensions,
          (Signal.map2 
            (\isDown (x, y) -> if isDown then MouseClick (toFloat x, toFloat y) else MouseMove (toFloat x, toFloat y)) 
            Mouse.isDown Mouse.position ),
          Signal.map KeyDown Keyboard.keysDown
           ]


main = Signal.map (view mouseMailbox.address) model'