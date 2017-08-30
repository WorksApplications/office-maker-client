module MapTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Page.Map.Model as Model exposing (Model)
import Page.Map.Update as Update exposing (Flags)
import API.Cache as Cache exposing (Cache, UserState)
import Model.EditingFloor as EditingFloor
import Model.Floor as Floor exposing (Floor)
import Model.Object as Object exposing (Object)
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
                            [ Initialize False Nothing userState ]
                        -- |> Debug.log "model"
                        |> Expect.all [ (\model -> Expect.pass) ]
            ]
        , describe "EditingFloor"
            [ test "works" <|
                \_ ->
                    EditingFloor.init
                        (Floor.init "floor1"
                            |> Floor.addObjects
                                [ Object.initDesk
                                    "object1"
                                    "floor1"
                                    (Position 0 0)
                                    (Size 100 100)
                                    "#000"
                                    "name"
                                    10.5
                                    Nothing
                                    Nothing
                                ]
                        )
                        |> EditingFloor.updateObjects (Floor.move [ "object1" ] 10 ( 20, 30 ))
                        |> Tuple.first
                        |> EditingFloor.undo
                        |> Tuple.second
                        |> Debug.log "changes"
                        |> (always Expect.pass)
            ]
        ]
