module SignalGraph where

import Diagrams.Core exposing (..)
import Diagrams.Pad as Pad
import Diagrams.FillStroke exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Query exposing (..)
import Graphics.Collage
import Text
import Color
import Dict exposing (Dict)

import SimpleGraph exposing (..)
import DataUtils exposing (..)
import Debugger.Model as DM


type alias Tag =
  DM.NodeId


viewNode : DM.NodeId -> DM.NodeInfo -> Diagram Tag a
viewNode id nodeInfo =
  text nodeInfo.name Text.defaultStyle
    |> Pad.pad 5
    |> Pad.background (fillAndStroke (Solid Color.lightBlue) Graphics.Collage.defaultLine)
    |> tag id


viewRow : List (DM.NodeId, DM.NodeInfo) -> Diagram Tag a
viewRow row =
  row
    |> List.map (\(id, nodeInfo) -> viewNode id nodeInfo)
    |> List.intersperse (hspace 10)
    |> hcat
    |> alignCenter


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
