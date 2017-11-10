module Model.SaveRequest exposing (..)

import CoreType exposing (..)
import Model.EditingFloor exposing (EditingFloor)
import Model.Floor exposing (Floor)
import Model.ObjectsChange as ObjectsChange exposing (ObjectsChange)


type SaveRequest
    = SaveFloor Floor
    | PublishFloor FloorId
    | SaveObjects ObjectsChange


type alias ReducedSaveRequest =
    { floor : Maybe Floor
    , publish : Maybe FloorId
    , objects : ObjectsChange
    }


emptyReducedSaveRequest : ReducedSaveRequest
emptyReducedSaveRequest =
    { floor = Nothing
    , publish = Nothing
    , objects = ObjectsChange.empty
    }


reduceRequest : Maybe EditingFloor -> List SaveRequest -> ReducedSaveRequest
reduceRequest maybeEditingFloor list =
    case maybeEditingFloor of
        Just _ ->
            List.foldr reduceRequestHelp emptyReducedSaveRequest list

        Nothing ->
            emptyReducedSaveRequest


reduceRequestHelp : SaveRequest -> ReducedSaveRequest -> ReducedSaveRequest
reduceRequestHelp req reducedSaveRequest =
    case req of
        SaveFloor floor ->
            { reducedSaveRequest
                | floor = Just floor
            }

        PublishFloor floorId ->
            { reducedSaveRequest
                | publish = Just floorId
            }

        SaveObjects objectsChange ->
            { reducedSaveRequest
                | objects =
                    ObjectsChange.merge
                        objectsChange
                        reducedSaveRequest.objects
            }
