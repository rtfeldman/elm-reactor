module Debugger.Reflect where

import Html
import Json.Encode as JsEnc
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)

import Native.Debugger.Reflect


type ElmValue
  -- literals
  = Number Int -- ???
  | Chr Char
  | Str String
  | Boolean Bool
  -- special types
  | ListV (List ElmValue)
  | DictV (List (ElmValue, ElmValue))
  | SetV (List ElmValue)
  | ArrayV (List ElmValue)
  | TupleV (List ElmValue)
  -- other
  | Constructor String (List ElmValue)
  | Record (List (String, ElmValue)) -- TODO: wut is really going on here
  | Function String
  -- when we give up
  | NativeVal JsEnc.Value
  | SignalV


type alias JsElmValue =
  JsEnc.Value


getHtml : JsElmValue -> Html.Html
getHtml =
  Native.Debugger.Reflect.getHtml


reflect : a -> ElmValue
reflect =
  jsRepr >> decode


jsRepr : a -> JsElmValue
jsRepr =
  Native.Debugger.Reflect.jsRepr


decode : JsElmValue -> ElmValue
decode =
  Native.Debugger.Reflect.decode
