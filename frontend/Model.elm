module Model where

import Json.Decode exposing (..)

import WebSocket
import Html.File as File

import Debugger.Service as Service
import Debugger.Active as Active
import SideBar.Logs as Logs
import Button
import Debugger.Model as DM
import SignalGraph


type alias Model =
  { serviceState : Service.Model
  , sidebarVisible : Bool
  , permitSwaps : Bool
  , restartButtonState : Button.Model
  , playPauseButtonState : Button.Model
  , errorState : ErrorState
  , logsState : Logs.Model
  , swapSocket : Maybe WebSocket.WebSocket
  , signalGraphState : SignalGraph.Model
  }


type ErrorState
  = CompilationErrors CompilationErrors
  | SwapReplayError DM.ReplayError
  | HistoryMismatchError
      { currentModuleName : DM.ModuleName
      , historyModuleName : DM.ModuleName
      }
  | SessionInputError SessionInputError
  | NoErrors


type SessionInputError
  = IoError File.IoError
  | ParseError String


initModel : Model
initModel =
  { serviceState = Service.initModel
  , sidebarVisible = True
  , permitSwaps = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  , errorState = NoErrors
  , logsState = Logs.initModel
  , swapSocket = Nothing
  , signalGraphState = SignalGraph.initState
  }


type Message
  = SidebarVisible Bool
  | PermitSwaps Bool
  | PlayPauseButtonAction (Button.Message Active.Command)
  | RestartButtonAction (Button.Message Active.Command)
  | LogsMessage Logs.Message
  | ConnectSocket (Maybe WebSocket.WebSocket)
  | ExportSession
  | ImportSession (List File.File)
  | SessionInputErrorMessage SessionInputError
  | SwapEvent SwapEvent
  | NewServiceState Service.Model
  | ServiceCommand Active.Command
  -- vv `Nothing` here means resetting the mismatch error
  | CommandResponse Active.CommandResponseMessage
  | CloseErrors
  | NoOp


type alias CompilationErrors =
  String


type SwapEvent
  = NewModuleEvent DM.CompiledElmModule
  | CompilationErrorsEvent CompilationErrors


swapEvent : Decoder SwapEvent
swapEvent =
  oneOf
    [ object2
        (\name code ->
            NewModuleEvent {name=name, code=code})
        ("name" := string)
        ("code" := string)
    , object1
        CompilationErrorsEvent
        ("error" := string)
    ]
