module Debugger.ReflectTest where

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import Array
import Set
import Signal
import Debug
import String

import TreeView
import Debugger.Reflect as Reflect
import Components exposing (..)


type alias Model =
  { val : Reflect.ElmValue }


initModel =
  { val =
      { x = 2
      , y = "Foo"
      , z = [Just 1,Nothing,Just 3]
      , a = Dict.fromList [('a', 3), ('b', 4)]
      , b = Set.fromList [1,2,3]
      , d = Array.fromList [1,2,3]
      , e = Just "sup"
      }
      |> Reflect.reflect
      |> Debug.log "val"
  }


app : App TreeView.Message Model
app =
  { init = done initModel
  , view = view
  , update = update
  , externalMessages = []
  }


output =
  Components.start app


main =
  output.html


update : TreeView.Message -> Model -> Transaction TreeView.Message Model
update msg state =
  done state


view : Signal.Address TreeView.Message -> Model -> Html
view addr state =
  let
    tree =
      toTree state.val
  in
    TreeView.view addr render tree (TreeView.allExpanded tree)


type ValueView
  = ContainerHeading Reflect.ElmValue
  | OrderedItem Int ValueView
  | RecordItem String ValueView
  | DictItemHeading Reflect.ElmValue Reflect.ElmValue
  | DictKey ValueView
  | DictValue ValueView
  | AtomValue Reflect.ElmValue


toTree : Reflect.ElmValue -> TreeView.Tree ValueView
toTree elmVal =
  let
    -- TODO dedupe
    mkOrdItem idx val =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (OrderedItem idx view)

        TreeView.Node view children ->
          TreeView.Node
            (OrderedItem idx view)
            children

    mkRecordItem (name, val) =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (RecordItem name view)

        TreeView.Node view children ->
          TreeView.Node
            (RecordItem name view)
            children

    mkDictKey val =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (DictKey view)

        TreeView.Node view children ->
          TreeView.Node
            (DictKey view)
            children

    mkDictVal val =
      case toTree val of
        TreeView.Leaf view ->
          TreeView.Leaf (DictValue view)

        TreeView.Node view children ->
          TreeView.Node
            (DictValue view)
            children

  in
    case elmVal of
      Reflect.ListV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem items)

      Reflect.DictV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (items |> List.map (\(k, v) ->
            TreeView.Node
              (DictItemHeading k v)
              [ mkDictKey k
              , mkDictVal v
              ]
          ))

      Reflect.SetV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem items)

      Reflect.ArrayV items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem items)

      Reflect.TupleV args ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem args)

      Reflect.Constructor ctor args ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.indexedMap mkOrdItem args)

      Reflect.Record items ->
        TreeView.Node
          (ContainerHeading elmVal)
          (List.map mkRecordItem items)

      _ ->
        elmVal
          |> AtomValue
          |> TreeView.Leaf


(=>) = (,)


render : TreeView.RenderFun ValueView
render valueView expanded =
  case valueView of
    ContainerHeading val ->
      case val of
        Reflect.ListV items ->
          text "List"

        Reflect.DictV items ->
          text "Dict"

        Reflect.SetV items ->
          text "Set"

        Reflect.ArrayV items ->
          text "Array"

        Reflect.TupleV items ->
          text "Tuple"

        Reflect.Record items ->
          text "record"

        Reflect.Constructor ctor items ->
          text ctor

        _ ->
          Debug.crash <| "not a container: " ++ (toString val)

    OrderedItem idx view ->
      span []
        [ text <| toString idx ++ ": "
        , render view expanded
        ]

    RecordItem name view ->
      span []
        [ text <| name ++ " = "
        , render view expanded
        ]

    DictItemHeading keyVal valVal ->
      text "…"

    DictKey view ->
      span []
        [ text "key: "
        , render view expanded
        ]

    DictValue view ->
      span []
        [ text "val: "
        , render view expanded
        ]

    AtomValue val ->
      case val of
        Reflect.Number int ->
          span
            [ style ["color" => "blue"] ]
            [ text <| toString int ]

        Reflect.Chr chr ->
          span
            [ style ["color" => "green"] ]
            [ text <| "'" ++ String.fromChar chr ++ "'" ]

        Reflect.Str str ->
          -- TODO add slashes
          span
            [ style ["color" => "green"] ]
            [ text <| "\"" ++ str ++ "\"" ]

        Reflect.Boolean bool ->
          span
            [ style ["color" => "blue"] ]
            [ text <| if bool then "True" else "False" ]

        _ ->
          Debug.crash <| "not an atom: " ++ toString val
