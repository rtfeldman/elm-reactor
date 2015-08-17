module SimpleGraph where

import Dict exposing (Dict)
import Debug
import DataUtils exposing (..)


type alias DiGraph comparable b =
  Dict comparable (b, List comparable)


longestPathFrom : comparable -> DiGraph comparable b -> Int
longestPathFrom node graph =
  let
    go node depths =
      case Dict.get node depths of
        Just depth ->
          (depth, depths)

        Nothing ->
          let
            (maxChildDepth, updatedDepths) =
              graph
                |> Dict.get node
                |> getMaybe "node not found"
                |> Debug.log "node"
                |> snd
                |> List.foldl
                    (\label (depth, depths) ->
                      let
                        (newDepth, newDepths) =
                          go label depths
                      in
                        ( max newDepth depth
                        , newDepths
                        ))
                    (0, depths)
          in
            (maxChildDepth + 1, Dict.insert node (maxChildDepth + 1) updatedDepths)
  in
    go node Dict.empty |> fst


addEdge : comparable -> comparable -> DiGraph comparable b -> DiGraph comparable b
addEdge from to graph =
  let
    update maybeNode =
      case maybeNode of
        Just (node, kids) ->
          Just (node, to :: kids)

        Nothing ->
          Debug.crash <| "from node not found" ++ toString (from, to, graph)
  in
    Dict.update from update graph


reverse : DiGraph comparable b -> DiGraph comparable b
reverse graph =
  let
    justNodes =
      Dict.map (\nodeId (node, _) -> (node, [])) graph

    step nodeId (node, kids) revGraph =
      List.foldl (\kidId gr -> addEdge kidId nodeId gr) revGraph kids
  in
    Dict.foldl step justNodes graph


prune : DiGraph comparable b -> DiGraph comparable b
prune graph =
  let
    reversed =
      reverse graph

    isOrphan nodeId _ =
      List.isEmpty (edgesFrom nodeId graph)
        && List.isEmpty (edgesFrom nodeId reversed)
  in
    Dict.filter (\k v -> not (isOrphan k v)) graph


edgesFrom : comparable -> DiGraph comparable b -> List comparable
edgesFrom node graph =
  Dict.get node graph
    |> getMaybe "node not found"
    |> snd


heightLevels : DiGraph comparable b -> Dict Int (List (comparable, b))
heightLevels graph =
  graph
    |> Dict.toList
    |> List.map (\(id, (node, outEdges)) -> (longestPathFrom id graph, (id, node)))
    |> groupBy


groupBy : List (comparable, b) -> Dict comparable (List b)
groupBy list =
  let
    update (key, val) dict =
      dict |> Dict.update key (\existing ->
        case existing of
          Just list ->
            Just (val :: list)

          Nothing ->
            Just [val])
  in
    List.foldl update Dict.empty list
