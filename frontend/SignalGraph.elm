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
import Color
import Dict exposing (Dict)

import Effects exposing (..)

import SimpleGraph exposing (..)
import DataUtils exposing (..)
import Debugger.Model as DM


type alias Tag =
  DM.NodeId


type Action
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


update : Action -> Model -> (Model, Effects Action)
update msg state =
  case msg of
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


view : Model -> DM.SGShape -> Html
view model sgShape =
  sgShape
    |> .nodes
    |> Dict.map (\nodeId nodeInfo -> (nodeInfo, nodeInfo.kids))
    |> viewGraph
    |> render
    |> (\x -> [x])
    |> Graphics.Collage.collage 300 400
    |> Html.fromElement


viewGraph : DiGraph DM.NodeId DM.NodeInfo -> Diagram Tag a
viewGraph graph =
  let
    pruned =
      prune graph

    nodes =
      (heightLevels pruned)
        |> Dict.toList
        |> List.reverse
        |> List.map (snd >> viewRow)
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


viewRow : List (DM.NodeId, DM.NodeInfo) -> Diagram Tag a
viewRow row =
  row
    |> List.map (\(id, nodeInfo) -> viewNode id nodeInfo)
    |> List.intersperse (hspace 10)
    |> hcat
    |> alignCenter


viewNode : DM.NodeId -> DM.NodeInfo -> Diagram Tag a
viewNode id nodeInfo =
  text nodeInfo.name Text.defaultStyle
    |> Pad.pad 5
    |> Pad.background (fillAndStroke (Solid Color.lightBlue) Graphics.Collage.defaultLine)
    |> tag id
