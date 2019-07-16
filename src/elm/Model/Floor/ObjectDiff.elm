module Model.Floor.ObjectDiff exposing
    ( ObjectDiff
    , ObjectDiffProps
    , at
    , collect
    , decoder
    , extractJson
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode


type alias ObjectDiffProps =
    { id : String, floorId : String, data : Json.Encode.Value }


extractJson : ObjectDiffProps -> Json.Encode.Value
extractJson diff =
    diff.data


at : ObjectDiffProps -> String -> Decoder a -> Maybe a
at diff field decoder =
    Result.toMaybe <| D.decodeValue (D.field field decoder) diff.data


{-| The raw representation for ObjectChange
This data only contains the updated fields (the non-updated fields are all null)

This is mainly used in the process of editObjectSubscription

-}
type ObjectDiff
    = ObjectDiff { flag : String, object : ObjectDiffProps }


decoder : Decoder ObjectDiff
decoder =
    let
        objectDecoder =
            D.value
                |> D.andThen
                    (\value ->
                        let
                            tryId =
                                D.decodeValue (D.field "id" D.string) value

                            tryFloorId =
                                D.decodeValue (D.field "floorId" D.string) value

                            tryObject =
                                Result.map2 (\id floorId -> { id = id, floorId = floorId, data = value })
                                    tryId
                                    tryFloorId
                        in
                        case tryObject of
                            Err err ->
                                D.fail ("Cannot decode into ObjectDiff: " ++ toString err)

                            Ok object ->
                                D.succeed object
                    )
    in
    decode (\flag object -> ObjectDiff { flag = flag, object = object })
        |> required "flag" D.string
        |> required "object" objectDecoder


collect : List ObjectDiff -> { added : List ObjectDiffProps, modified : List ObjectDiffProps, deleted : List ObjectDiffProps }
collect diffs =
    List.foldl
        (\(ObjectDiff diff) acc ->
            case diff.flag of
                "added" ->
                    { acc | added = diff.object :: acc.added }

                "modified" ->
                    { acc | modified = diff.object :: acc.modified }

                "deleted" ->
                    { acc | modified = diff.object :: acc.deleted }

                _ ->
                    Debug.log ("Unsupported flag in ObjectDiff: " ++ diff.flag) acc
        )
        { added = [], modified = [], deleted = [] }
        diffs
