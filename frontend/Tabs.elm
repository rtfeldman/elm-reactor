module Tabs where

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Debug

import DataUtils exposing (..)


type alias Model b =
  { currentTab : Int
  , tabs : Array b
  }


type Action
  = SwitchToTab Int
  | CloseTab Int


update : Action -> Model a -> Model a
update action state =
  let
    inBounds idx =
      idx > 0 && idx < Array.length state.tabs
  in
    case action of
      SwitchToTab idx ->
        if inBounds idx then
          { state | currentTab <- idx }
        else
          Debug.crash "tab index out of range"

      CloseTab idx ->
        if inBounds idx then
          { state | tabs <- state.tabs }
        else
          Debug.crash "tab index out of range"


type alias ViewConfig a =
  { renderBody : a -> Html
  , renderTab : Int -> a -> Html
  , tabBarStyle : List (String, String)
  , tabStyle : List (String, String)
  , tabBodyStyle : List (String, String)
  }


render : Model a -> ViewConfig a -> Html
render state viewConfig =
  div
    []
    [ ul
        [ style viewConfig.tabBarStyle ]
        (state.tabs |> Array.indexedMap viewConfig.renderTab |> Array.toList)
    , div
        [ style viewConfig.tabBodyStyle ]
        [ state.tabs |> Array.get state.currentTab |> getMaybe "tab idx out of range" |> viewConfig.renderBody ]
    ]
