module Page.Master.Msg exposing (..)

import API.Cache exposing (UserState)
import Component.Header as Header
import Debounce
import Http
import Model.ColorPalette exposing (ColorPalette)
import Model.Prototype exposing (Prototype)
import Model.User exposing (User)
import Page.Master.PrototypeForm exposing (PrototypeForm)


type Msg
    = NoOp
    | Loaded UserState User ColorPalette (List Prototype)
    | HeaderMsg Header.Msg
    | AddColor Bool
    | DeleteColor Bool Int
    | InputColor Bool Int String
    | UpdatePrototype Int PrototypeForm
    | SaveColorDebounceMsg Debounce.Msg
    | SavePrototypeDebounceMsg Debounce.Msg
    | NotAuthorized
    | APIError Http.Error
    | DeletePrototype Int PrototypeForm
