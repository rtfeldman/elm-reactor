module TreeView where

import Dict exposing (Dict)
import Signal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug

import Transaction exposing (..)

type alias Model a =
  { expansionModel : ExpansionModel
  , tree : Tree a
  }


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


update : Message -> Model a -> Transaction Message (Model a)
update msg state =
  case msg of
    ChangeExpansion expanded ->
      case (state.tree, state.expansionModel) of
        (Node _ _, ExpNode _ expChildren) ->
          done { state | expansionModel <- ExpNode expanded expChildren }

        (Leaf _, ExpLeaf) ->
          Debug.crash "trying to change expansion of a leaf"

        _ ->
          Debug.crash "mismatched expansion model and tree"

    ChildMessage idx msg ->
      case (state.tree, state.expansionModel) of
        (Node _ children, ExpNode expanded expChildren) ->
          let
            subUpdate childIdx (subTree, subExpModel) =
              if childIdx == idx then
                with
                  (tag
                    (ChildMessage idx)
                    (update msg { tree = subTree, expansionModel = subExpModel }))
                  done
              else
                done { tree = subTree, expansionModel = subExpModel }
          in
            with
              (Transaction.list
                (List.indexedMap
                  subUpdate <|
                    List.map2 (,) children expChildren))
              (\newChildModels ->
                done
                  { state | expansionModel <-
                      ExpNode expanded (List.map .expansionModel newChildModels)
                  }
              )

        (Leaf _, ExpLeaf) ->
          Debug.crash "child message on a leaf"

        _ ->
          Debug.crash "mismatched expansion model and tree"


view : Signal.Address Message -> RenderFun a -> Model a -> Html
view addr render state =
  case (state.tree, state.expansionModel) of
    (Leaf item, ExpLeaf) ->
      render item False

    (Node item children, ExpNode expanded expChildren) ->
      let
        heading =
          [ span
              [ onClick addr (ChangeExpansion (not expanded))
              , style
                  ["cursor" => "default"]
              ]
              [ text <| if expanded then "▼" else "▶" ]
          , span [] [ render item expanded ]
          ]

        childrenViews =
          if expanded then
            [ ul
                [ style
                    [ "list-style" => "none"
                    , "margin-top" => "0"
                    , "padding-left" => "20px"
                    ]
                ]
                (List.map2 (,) children expChildren
                  |> List.indexedMap (\idx (child, expChild) ->
                    li
                      []
                      [ view
                          (Signal.forwardTo addr (ChildMessage idx))
                          render
                          { tree = child
                          , expansionModel = expChild
                          }
                      ]
                  )
                )
            ]
          else
            []
      in
        div [] (heading ++ childrenViews)

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
