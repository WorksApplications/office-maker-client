module Page.Master.Msg exposing (..)

import Http
import Debounce
import API.Cache exposing (UserState)
import Component.Header as Header
import Model.User exposing (User)
import Model.Prototype exposing (Prototype)
import Model.ColorPalette exposing (ColorPalette)
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
