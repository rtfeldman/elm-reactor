module SignalGraph where

import Diagrams.Core exposing (..)
import Diagrams.Pad as Pad
import Diagrams.FillStroke exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Query exposing (..)
import Graphics.Collage
import Html exposing (Html)
import Text
import Time exposing (Time)
import Color exposing (Color)
import Dict exposing (Dict)

import Effects exposing (..)
import Easing

import SimpleGraph exposing (..)
import DataUtils exposing (..)
import Debugger.Model as DM


type alias Tag =
  DM.NodeId


type Message
  = Pulse (List DM.NodeId)
  | Tick Time


type alias Model =
  { nodes : List DM.NodeId
  , animationState : AnimationState
  }


duration = Time.second


type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time }


initState : Model
initState =
  { animationState = Nothing
  , nodes = []
  }


update : Message -> Model -> (Model, Effects Message)
update action state =
  case action of
    Pulse nodes ->
      case state.animationState of
        Nothing ->
          ( { state | nodes <- nodes }
          , Effects.tick Tick
          )

        Just animState ->
          ( { state
                | animationState <- Just { animState | elapsedTime <- 0 }
                , nodes <- nodes
            }
          , Effects.none
          )

    Tick clockTime ->
      let
        newElapsedTime =
          case state.animationState of
            Nothing ->
              0

            Just {elapsedTime, prevClockTime} ->
              elapsedTime + (clockTime - prevClockTime)
      in
        if newElapsedTime > duration then
          ( { state
                | animationState <- Nothing
            }
          , Effects.none
          )
        else
          ( { state
                | animationState <-
                    Just
                      { elapsedTime = newElapsedTime
                      , prevClockTime = clockTime
                      }
            }
          , Effects.tick Tick
          )


normalColor =
  Color.lightBlue


activeColor =
  Color.red


view : Model -> DM.SGShape -> Html
view model sgShape =
  let
    pulseColor =
      case model.animationState of
        Nothing ->
          normalColor

        Just {elapsedTime} ->
          Easing.ease
            Easing.easeOutQuad
            Easing.color
            activeColor
            normalColor
            duration
            elapsedTime

  in
    sgShape
      |> .nodes
      |> Dict.map (\nodeId nodeInfo -> (nodeInfo, nodeInfo.kids))
      |> viewGraph pulseColor model.nodes
      |> render
      |> (\x -> [x])
      |> Graphics.Collage.collage 300 400
      |> Html.fromElement


viewGraph : Color -> List DM.NodeId -> DiGraph DM.NodeId DM.NodeInfo -> Diagram Tag a
viewGraph pulseColor activeNodes graph =
  let
    pruned =
      prune graph

    nodes =
      (heightLevels pruned)
        |> Dict.toList
        |> List.reverse
        |> List.map (snd >> (viewRow pulseColor activeNodes))
        |> List.intersperse (vspace 20)
        |> vcat

    edges =
      pruned
        |> Dict.toList
        |> List.map (\(id, (_, outEdges)) ->
              outEdges
                |> List.map (viewEdge id)
                |> zcat)
        |> zcat

    viewEdge fromId toId =
      let
        fromPoint =
          getCoords nodes [fromId]
            |> getMaybe "node not found"

        toPoint =
          getCoords nodes [toId]
            |> getMaybe "node not found"
      in
        path [fromPoint, toPoint] Graphics.Collage.defaultLine

  in
    zcat [nodes, edges]
      |> alignCenter


viewRow : Color -> List DM.NodeId -> List (DM.NodeId, DM.NodeInfo) -> Diagram Tag a
viewRow pulseColor activeNodes row =
  row
    |> List.map (\(id, nodeInfo) -> viewNode pulseColor activeNodes id nodeInfo)
    |> List.intersperse (hspace 10)
    |> hcat
    |> alignCenter


viewNode : Color -> List DM.NodeId -> DM.NodeId -> DM.NodeInfo -> Diagram Tag a
viewNode pulseColor activeNodes id nodeInfo =
  let
    bgColor =
      if activeNodes |> List.member id then
        pulseColor
      else
        normalColor
  in
    text nodeInfo.name Text.defaultStyle
      |> Pad.pad 5
      |> Pad.background (fillAndStroke (Solid bgColor) Graphics.Collage.defaultLine)
      |> tag id
