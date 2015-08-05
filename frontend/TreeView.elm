module TreeView where

import Dict exposing (Dict)
import Signal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug


type Tree a
  = Node a (List (Tree a))
  | Leaf a


type ExpansionModel
  = ExpNode Bool (List ExpansionModel) -- True = expanded
  | ExpLeaf


{- item, expanded. Always false for leaf nodes. -}
type alias RenderFun a =
  a -> Bool -> Html


type Message
  = ChangeExpansion Bool
  | ChildMessage Int Message


(=>) = (,)


view : (Signal.Address Message) -> RenderFun a -> Tree a -> ExpansionModel -> Html
view addr render tree expModel =
  case (tree, expModel) of
    (Leaf item, ExpLeaf) ->
      render item False

    (Node item children, ExpNode expanded expChildren) ->
      div []
        [ span
            [ onClick addr (ChangeExpansion (not expanded))
            , style
                ["cursor" => "default"]
            ]
            [ text <| if expanded then "▼" else "▶" ]
        , span [] [ render item expanded ]
        , ul
            [ style
                [ "list-style" => "none"
                , "margin-top" => "0"
                ]
            ]
            (List.map2 (,) children expChildren
              |> List.indexedMap (\idx (child, expChild) ->
                li
                  []
                  [ view
                      (Signal.forwardTo addr (ChildMessage idx))
                      render
                      child
                      expChild
                  ]
              )
            )
        ]

    _ ->
      Debug.crash "mismatched tree and expansion model"


allExpanded : Tree a -> ExpansionModel
allExpanded tree =
  case tree of
    Leaf _ ->
      ExpLeaf

    Node _ children ->
      children
        |> List.map allExpanded
        |> ExpNode True


allCollapsed : Tree a -> ExpansionModel
allCollapsed tree =
  case tree of
    Leaf _ ->
      ExpLeaf

    Node _ children ->
      children
        |> List.map allCollapsed
        |> ExpNode False


emptyExpansionModel : ExpansionModel
emptyExpansionModel =
  ExpLeaf


updateExpansion : Tree a -> Bool -> ExpansionModel -> ExpansionModel
updateExpansion newTree default expModel =
  expModel
