module MapTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Page.Map.Model as Model exposing (Model)
import Page.Map.Update as Update exposing (Flags)
import API.Cache as Cache exposing (Cache, UserState)
import Model.User as User exposing (User)
import Page.Map.Msg exposing (Msg(..))
import Model.I18n as I18n exposing (Language(..))
import CoreType exposing (..)
import Navigation exposing (Location)


location : Location
location =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }


initialModel : Model
initialModel =
    Update.init
        { apiRoot = ""
        , cacheRoot = ""
        , accountServiceRoot = ""
        , profileServiceRoot = ""
        , imageRoot = ""
        , authToken = ""
        , title = ""
        , initialSize = Size 0 0
        , randomSeed = ( 0, 0 )
        , visitDate = 0.0
        , lang = "ja"
        }
        location
        |> Tuple.first


batchUpdate : List Msg -> Model -> Model
batchUpdate msgs model =
    msgs
        |> List.foldl
            (\msg model ->
                Update.update msg model
                    |> (\( m, c ) ->
                            let
                                _ =
                                    Debug.log "cmd" c
                            in
                                m
                       )
            )
            model


userState : UserState
userState =
    Cache.defaultUserState JA


suite : Test
suite =
    describe "Office Maker Client"
        [ describe "Map"
            [ test "works" <|
                \_ ->
                    initialModel
                        |> batchUpdate
                            [ Initialized Nothing False userState User.guest ]
                        -- |> Debug.log "model"
                        |> Expect.all [ (\model -> Expect.pass) ]
            ]
        ]
