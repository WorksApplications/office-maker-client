module API.AuthToken exposing (Payload, decodePayload, decodeValidPayload)

import Date
import Native.JWT
import Task exposing (Task)


type alias Payload =
    { userId : String
    , role : String
    , exp : Int
    }


decodePayload : String -> Payload
decodePayload =
    Native.JWT.decodePayload


{-| Returns Nothing if the JWT is expired.
-}
decodeValidPayload : String -> Task String Payload
decodeValidPayload jwt =
    let
        payload =
            decodePayload jwt
    in
    Date.now
        |> Task.map Date.second
        |> Task.andThen
            (\now ->
                if payload.exp < now then
                    Task.succeed payload

                else
                    Task.fail "JWT expired"
            )
