module API.GraphQL exposing (EditObject, buildSimpleQuery, listEditObjectsOnFloor)

{-| This module provides GraphQL client for AppSync (see `schema.graphql`)
There is also a library called `elm-graphql`, you might want to use it instead. (I could not install it in 0.18)
-}

import API.Serialization
import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import Model.Object exposing (Object)


type alias Config =
    { apiGraphQLRoot : String
    , apiKey : String
    , token : String
    }


type alias EditObject =
    { backgroundColor : Maybe String
    , changed : Maybe Bool
    , deleted : Maybe Bool
    , floorId : String
    , height : Maybe Int
    , id : String
    , updateAt : Maybe Int
    , width : Maybe Int
    , x : Maybe Int
    , y : Maybe Int
    , name : Maybe String
    , personId : Maybe String
    , fontSize : Maybe Int
    , typeOf : Maybe String
    , url : Maybe String
    }


{-| Build a simple query, can be used for mutation

    buildSimpleQuery q {a -> xxx, b -> yyy} [id,updatedAt] = { q(a: "xxx", b: "yyy") { id updatedAt } }

-}
buildSimpleQuery : String -> Dict String String -> List String -> String
buildSimpleQuery query vars picks =
    let
        parameter =
            List.map (\( k, v ) -> k ++ ": \"" ++ v ++ "\"") (Dict.toList vars) |> String.concat

        response =
            List.foldl (\label acc -> label ++ " " ++ acc) "" picks
    in
    "{ " ++ query ++ "(" ++ parameter ++ ") { " ++ response ++ " } }"


listEditObjectsOnFloor : Config -> String -> Http.Request (List Object)
listEditObjectsOnFloor config floorId =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "x-api-key" config.apiKey
            , Http.header "Authorization" <| "Bearer " ++ config.token
            , Http.header "Content-Type" "application/graphql"
            ]
        , url = config.apiGraphQLRoot
        , body =
            Http.jsonBody <|
                Json.Encode.object <|
                    (\value -> [ ( "query", Json.Encode.string value ) ]) <|
                        buildSimpleQuery "listEditObjectsOnFloor"
                            (Dict.singleton "floorId" floorId)
                            [ "backgroundColor"
                            , "changed"
                            , "deleted"
                            , "floorId"
                            , "height"
                            , "id"
                            , "updateAt"
                            , "width"
                            , "x"
                            , "y"
                            , "name"
                            , "personId"
                            , "fontSize"
                            , "type"
                            , "url"
                            ]
        , expect =
            Http.expectJson <|
                Json.Decode.at [ "data", "listEditObjectsOnFloor" ] <|
                    Json.Decode.list API.Serialization.decodeObject
        , timeout = Nothing
        , withCredentials = False
        }
