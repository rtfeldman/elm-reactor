module Pulse where

import Easing
import Effects exposing (Effects, Never)
import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Styles exposing (colorToCss, (=>))
import Color
import Task


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks





-- MODEL

type alias Model =
    { animationState : AnimationState
    } -- 1 => 0


type alias AnimationState =
    Maybe { prevClockTime : Time,  elapsedTime : Time }


init : (Model, Effects Action)
init =
  ( { animationState = Nothing }
  , Effects.none
  )


duration = second


-- UPDATE

type Action
    = Pulse
    | Tick Time


update : Action -> Model -> (Model, Effects Action)
update msg model =
  case msg of
    Pulse ->
      case model.animationState of
        Nothing ->
          ( model, Effects.tick Tick )

        Just animState ->
          ( { model | animationState <- Just { animState | elapsedTime <- 0 } }
          , Effects.none
          )

    Tick clockTime ->
      let
        newElapsedTime =
          case model.animationState of
            Nothing ->
              0

            Just {elapsedTime, prevClockTime} ->
              elapsedTime + (clockTime - prevClockTime)
      in
        if newElapsedTime > duration then
          ( { animationState = Nothing
            }
          , Effects.none
          )
        else
          ( { animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
            }
          , Effects.tick Tick
          )


-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    color =
      case model.animationState of
        Nothing ->
          Color.white

        Just {elapsedTime} ->
          Easing.ease
            Easing.easeOutQuad
            Easing.color
            Color.red
            Color.white
            duration
            elapsedTime
  in
    div
      [ style [ "background-color" => colorToCss color ]
      , onClick address Pulse
      ]
      [ text "SUPPPP" ]


