module Debugger.ReflectTest where

import Html exposing (..)
import Dict
import Array
import Set

import Debugger.Reflect as Reflect

main =
  let
    val =
      { x=2
      , y="Foo"
      , z=[1,2,3]
      , a=Dict.fromList [('a', 3), ('b', 4)]
      , b=Set.fromList [1,2,3]
      , d=Array.fromList [1,2,3]
      }
  in
    val |> Reflect.reflect |> toString |> text
