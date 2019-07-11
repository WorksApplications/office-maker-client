module API.GraphQL exposing
    ( listEditObjectsOnFloor
    , loadParameterJson
    , runListEditObjectsOnFloor
    )

{-| This module provides GraphQL client for AppSync (see `schema.graphql`)
There is also a library called `elm-graphql`, you might want to use it instead. (I could not install it in 0.18)
-}

import API.Serialization
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode
import Model.Object exposing (Object)
import Model.ObjectsChange exposing (ObjectChange)
import Task exposing (Task)


type alias Config =
    { apiGraphQLRoot : String
    , apiKey : String
    , apiGraphQLParameter : String
    , token : String
    }


type alias Info =
    { url : String
    , key : String
    }


loadParameterJson : String -> Http.Request Info
loadParameterJson url =
    Http.get url
        (decode Info
            |> required "url" Json.Decode.string
            |> required "key" Json.Decode.string
        )


createAppSyncRequest : Config -> String -> Http.Expect a -> Http.Request a
createAppSyncRequest config query expect =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "x-api-key" config.apiKey
            , Http.header "Authorization" <| "Bearer " ++ config.token
            , Http.header "Content-Type" "application/graphql"
            ]
        , url = config.apiGraphQLRoot
        , body = Http.jsonBody <| Json.Encode.object [ ( "query", Json.Encode.string query ) ]
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


{-| (Ugly) workaround for direct reloading.
Currently, GraphQL Parameter will be loaded once when one opens the map page.
In this case, opening an edit page and reloading will cause an error in map page
(since loading edit floor and loading parameter will be both immediately executed when opening the page)
so if the GraphQL Parameter is not loaded yet, load it instantly.

If `Map.Update.update` could wait until the GraphQL parameter will be loaded or could perform some lazy loading,
that should be better.

This function also not modifying the model so this won't set the loaded parameter.
This may cause an inefficient many-time API calls.

-}
executeAppSyncQuery : Config -> Http.Request a -> Task Http.Error a
executeAppSyncQuery config request =
    (if config.apiGraphQLRoot == "" then
        Http.toTask (loadParameterJson config.apiGraphQLParameter)
            |> Task.map (\info -> { config | apiGraphQLRoot = info.url, apiKey = info.key })

     else
        Task.succeed config
    )
        |> Task.andThen (\config -> Http.toTask request)


listEditObjectsOnFloor : Config -> String -> Http.Request (List Object)
listEditObjectsOnFloor config floorId =
    createAppSyncRequest config
        ("""query Q {
            listEditObjectsOnFloor(floorId: """ ++ Json.Encode.encode 0 (Json.Encode.string floorId) ++ """) {
                backgroundColor
                changed
                deleted
                floorId
                height
                id
                updateAt
                width
                x
                y
                name
                personId
                fontSize
                type
                url
            }
        }""")
        (Http.expectJson <|
            Json.Decode.at [ "data", "listEditObjectsOnFloor" ] <|
                Json.Decode.list API.Serialization.decodeObject
        )


patchObjects : Config -> List ObjectChange -> Http.Request Json.Decode.Value
patchObjects config objects =
    createAppSyncRequest config
        ("""mutation M {
            patchObjects(objects: """ ++ Json.Encode.encode 0 (API.Serialization.encodeObjectsChange objects) ++ """) {
                updatedFloorId
                objects {
                    flag
                    object {
                        backgroundColor
                        changed
                        deleted
                        floorId
                        height
                        id
                        updateAt
                        width
                        x
                        y
                        name
                        personId
                        fontSize
                        type
                        url
                    }
                    result
                }
            }
        }""")
        (Http.expectJson <|
            Json.Decode.at [ "data", "patchObjects", "objects" ] Json.Decode.value
        )


runListEditObjectsOnFloor : Config -> String -> Task Http.Error (List Object)
runListEditObjectsOnFloor config floorId =
    executeAppSyncQuery config (listEditObjectsOnFloor config floorId)


runPatchObjects : Config -> List ObjectChange -> Task Http.Error Json.Decode.Value
runPatchObjects config objects =
    executeAppSyncQuery config (patchObjects config objects)
