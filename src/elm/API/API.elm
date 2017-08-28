module API.API
    exposing
        ( getAuth
        , search
        , searchEx
        , saveFloor
        , getObject
        , saveObjects
        , publishFloor
        , deleteEditingFloor
        , getEditingFloor
        , getFloor
        , getFloorOfVersion
        , getFloorsInfo
        , saveEditingImage
        , login
        , personCandidate
        , getDiffSource
        , getPerson
        , getPersonMaybe
        , getPeopleByFloorAndPost
        , getColors
        , saveColors
        , getPrototypes
        , savePrototype
        , savePrototypes
        , Config
        , Error
        )

import Http
import Task exposing (Task)
import Util.HttpUtil as HttpUtil exposing (..)
import Util.File exposing (File)
import CoreType exposing (..)
import Model.Floor as Floor exposing (Floor, FloorBase)
import Model.FloorInfo as FloorInfo exposing (FloorInfo)
import Model.User as User exposing (User)
import Model.Person exposing (Person)
import Model.Object as Object exposing (..)
import Model.Floor as Floor exposing (Floor)
import Model.Prototype exposing (Prototype)
import Model.SearchResult exposing (SearchResult)
import Model.ColorPalette exposing (ColorPalette)
import Model.ObjectsChange as ObjectsChange exposing (ObjectChange, ObjectModification)
import API.Serialization exposing (..)


type alias ImageId =
    String


type alias UserId =
    String


type alias Error =
    Http.Error


type alias Config =
    { apiRoot : String
    , cacheRoot : String
    , accountServiceRoot : String
    , profileServiceRoot : String
    , imageRoot : String
    , token : String
    }


authorization : String -> Http.Header
authorization token =
    HttpUtil.authorization ("Bearer " ++ token)


getObject : Config -> ObjectId -> Task Error (Maybe Object)
getObject config objectId =
    get decodeObject
        (makeUrl
            (config.apiRoot ++ "/objects/" ++ objectId)
            []
        )
        [ authorization config.token ]
        |> recover404


saveObjects : Config -> List (ObjectChange ObjectModification) -> Task Error ()
saveObjects config changes =
    patchJsonNoResponse
        (config.apiRoot ++ "/objects2")
        [ authorization config.token ]
        (Http.jsonBody <| encodeObjectsChange changes)


saveFloor : Config -> Floor -> Task Error FloorBase
saveFloor config floor =
    putJson
        decodeFloorBase
        (config.apiRoot ++ "/floors/" ++ floor.id)
        [ authorization config.token ]
        (Http.jsonBody <| encodeFloor floor)


publishFloor : Config -> FloorId -> Task Error Floor
publishFloor config floorId =
    putJson
        decodeFloor
        (config.apiRoot ++ "/floors/" ++ floorId ++ "/public")
        [ authorization config.token ]
        Http.emptyBody


deleteEditingFloor : Config -> FloorId -> Task Error ()
deleteEditingFloor config floorId =
    deleteJsonNoResponse
        (config.apiRoot ++ "/floors/" ++ floorId)
        [ authorization config.token ]
        Http.emptyBody


getEditingFloor : Config -> FloorId -> Task Error Floor
getEditingFloor config floorId =
    getWithoutCache
        decodeFloor
        (makeUrl (config.apiRoot ++ "/floors/" ++ floorId) [ ( "all", "true" ) ])
        [ authorization config.token ]


getFloor : Config -> FloorId -> Task Error Floor
getFloor config floorId =
    getWithoutCache
        decodeFloor
        (config.cacheRoot ++ "/floors/" ++ floorId)
        [ authorization config.token ]


getFloorOfVersion : Config -> FloorId -> Int -> Task Error Floor
getFloorOfVersion config floorId version =
    get
        decodeFloor
        (makeUrl
            (config.apiRoot ++ "/floors/" ++ floorId ++ "/" ++ toString version)
            []
        )
        [ authorization config.token ]


getFloorMaybe : Config -> String -> Task Error (Maybe Floor)
getFloorMaybe config id =
    getFloor config id
        |> recover404


getFloorsInfo : Config -> Task Error (List FloorInfo)
getFloorsInfo config =
    getWithoutCache
        decodeFloorInfoList
        (makeUrl (config.apiRoot ++ "/floors") [])
        [ authorization config.token ]


getPrototypes : Config -> Task Error (List Prototype)
getPrototypes config =
    getWithoutCache
        decodePrototypes
        (makeUrl (config.apiRoot ++ "/prototypes") [])
        [ authorization config.token ]


savePrototypes : Config -> List Prototype -> Task Error ()
savePrototypes config prototypes =
    putJsonNoResponse
        (config.apiRoot ++ "/prototypes")
        [ authorization config.token ]
        (Http.jsonBody <| encodePrototypes prototypes)


savePrototype : Config -> Prototype -> Task Error ()
savePrototype config prototype =
    putJsonNoResponse
        (config.apiRoot ++ "/prototypes/" ++ prototype.id)
        [ authorization config.token ]
        (Http.jsonBody <| encodePrototype prototype)


getColors : Config -> Task Error ColorPalette
getColors config =
    getWithoutCache
        decodeColors
        (makeUrl (config.apiRoot ++ "/colors") [])
        [ authorization config.token ]


saveColors : Config -> ColorPalette -> Task Error ()
saveColors config colorPalette =
    putJsonNoResponse
        (config.apiRoot ++ "/colors")
        [ authorization config.token ]
        (Http.jsonBody <| encodeColorPalette colorPalette)


getDiffSource : Config -> String -> Task Error ( Floor, Maybe Floor )
getDiffSource config id =
    getEditingFloor config id
        |> Task.andThen
            (\current ->
                getFloorMaybe config id
                    |> Task.map (\prev -> ( current, prev ))
            )


getAuth : Config -> Task Error User
getAuth config =
    if String.trim config.token == "" then
        Task.succeed User.guest
    else
        getWithoutCache
            decodeUser
            (config.apiRoot ++ "/self")
            [ authorization config.token ]


search : Config -> Bool -> String -> Task Error ( List SearchResult, List Person )
search config withPrivate query =
    let
        url =
            makeUrl
                (config.apiRoot ++ "/search/" ++ Http.encodeUri query)
                (if withPrivate then
                    [ ( "all", "true" ) ]
                 else
                    []
                )
    in
        HttpUtil.get
            decodeSearchedPeopleWithObjectsAsSearchResults
            url
            [ authorization config.token ]


searchEx : Config -> Bool -> String -> Task Error ( List SearchResult, List Person )
searchEx config withPrivate query =
    let
        url =
            makeUrl
                (config.apiRoot ++ "/search-ex/" ++ Http.encodeUri query)
                (if withPrivate then
                    [ ( "all", "true" ) ]
                 else
                    []
                )
    in
        HttpUtil.get
            decodeSearchedObjectsAsSearchResults
            url
            [ authorization config.token ]


personCandidate : Config -> String -> Task Error (List Person)
personCandidate config name =
    if String.isEmpty name then
        Task.succeed []
    else
        HttpUtil.get
            decodePeopleFromProfileServiceSearch
            (makeUrl
                (config.profileServiceRoot ++ "/profiles")
                [ ( "q", String.join "" <| String.split "/" name )
                ]
            )
            [ authorization config.token ]


saveEditingImage : Config -> ImageId -> File -> Task a ()
saveEditingImage config imageId file =
    HttpUtil.sendFile
        "PUT"
        (config.apiRoot ++ "/images/" ++ imageId)
        [ authorizationTuple ("Bearer " ++ config.token) ]
        file


getPerson : Config -> PersonId -> Task Error Person
getPerson config personId =
    HttpUtil.get
        decodePersonFromProfileService
        (config.profileServiceRoot ++ "/profiles/" ++ personId)
        [ authorization config.token ]


getPersonMaybe : Config -> PersonId -> Task Error (Maybe Person)
getPersonMaybe config personId =
    getPerson config personId
        |> recover404


getPeopleByFloorAndPost : Config -> FloorId -> String -> Task Error (List Person)
getPeopleByFloorAndPost config floorId post =
    HttpUtil.get
        decodePeople
        (makeUrl
            (config.apiRoot ++ "/people")
            [ ( "floorId", floorId )
            , ( "post", post )
            ]
        )
        [ authorization config.token ]


login : String -> String -> String -> Task Error String
login accountServiceRoot id pass =
    postJson
        decodeAuthToken
        (accountServiceRoot ++ "/authentication")
        []
        (Http.jsonBody <| encodeLogin id pass)
