module API.AuthToken exposing (Payload, decodePayload)

import Native.JWT


type alias Payload =
    { userId : String
    , role : String
    }


decodePayload : String -> Payload
decodePayload =
    Native.JWT.decodePayload
