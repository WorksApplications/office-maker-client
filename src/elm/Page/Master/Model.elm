module Page.Master.Model exposing (..)

import API.API as API
import Component.Header as Header
import Debounce exposing (Debounce)
import Model.ColorPalette exposing (ColorPalette)
import Model.I18n exposing (Language(..))
import Model.Prototype exposing (Prototype)
import Page.Master.PrototypeForm exposing (PrototypeForm)


type alias Model =
    { apiConfig : API.Config
    , title : String
    , colorPalette : ColorPalette
    , prototypes : List PrototypeForm
    , error : Maybe String
    , header : Header.Model
    , lang : Language
    , saveColorDebounce : Debounce ColorPalette
    , savePrototypeDebounce : Debounce Prototype
    }
