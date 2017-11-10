module Page.Map.Main exposing (..)

import Html.Lazy exposing (lazy)
import Navigation
import Page.Map.Model exposing (Model)
import Page.Map.Msg exposing (Msg)
import Page.Map.Update as Update exposing (Flags)
import Page.Map.View as View


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Update.parseURL
        { init = Update.init
        , view = lazy View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
