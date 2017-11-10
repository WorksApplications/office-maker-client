module Model.FloorInfo
    exposing
        ( FloorInfo
        , editingFloor
        , findFloor
        , findPublicFloor
        , idOf
        , init
        , intToOrd
        , isNeverPublished
        , mergeEditingFloor
        , mergePublicFloor
        , publicFloor
        , sortByPublicOrder
        , toEditingList
        , toPublicList
        , toValues
        )

import CoreType exposing (..)
import Dict exposing (Dict)
import Model.Floor exposing (FloorBase)


type FloorInfo
    = FloorInfo (Maybe FloorBase) FloorBase


init : Maybe FloorBase -> FloorBase -> FloorInfo
init publicFloor editingFloor =
    FloorInfo publicFloor editingFloor


isNeverPublished : FloorInfo -> Bool
isNeverPublished floorsInfo =
    case floorsInfo of
        FloorInfo Nothing _ ->
            True

        _ ->
            False


idOf : FloorInfo -> FloorId
idOf (FloorInfo publicFloor editingFloor) =
    editingFloor.id


publicFloor : FloorInfo -> Maybe FloorBase
publicFloor (FloorInfo publicFloor editingFloor) =
    publicFloor


editingFloor : FloorInfo -> FloorBase
editingFloor (FloorInfo publicFloor editingFloor) =
    editingFloor


findPublicFloor : FloorId -> Dict FloorId FloorInfo -> Maybe FloorBase
findPublicFloor floorId floorsInfo =
    floorsInfo
        |> findFloor floorId
        |> Maybe.andThen publicFloor


findFloor : FloorId -> Dict FloorId FloorInfo -> Maybe FloorInfo
findFloor floorId floorsInfo =
    floorsInfo
        |> Dict.get floorId


mergeEditingFloor : FloorBase -> Dict FloorId FloorInfo -> Dict FloorId FloorInfo
mergeEditingFloor editingFloor floorsInfo =
    floorsInfo
        |> Dict.update editingFloor.id (Maybe.map (setEditingFloor editingFloor))


mergePublicFloor : FloorBase -> Dict FloorId FloorInfo -> Dict FloorId FloorInfo
mergePublicFloor publicFloor floorsInfo =
    floorsInfo
        |> Dict.update publicFloor.id (Maybe.map (setPublicFloor publicFloor))


setEditingFloor : FloorBase -> FloorInfo -> FloorInfo
setEditingFloor floor (FloorInfo publicFloor editingFloor) =
    FloorInfo publicFloor floor


setPublicFloor : FloorBase -> FloorInfo -> FloorInfo
setPublicFloor floor (FloorInfo publicFloor editingFloor) =
    FloorInfo (Just floor) editingFloor


toValues : Dict FloorId FloorInfo -> List FloorInfo
toValues floorsInfo =
    floorsInfo
        |> Dict.toList
        |> List.map Tuple.second


toPublicList : Dict FloorId FloorInfo -> List FloorBase
toPublicList floorsInfo =
    floorsInfo
        |> toValues
        |> List.filterMap publicFloor
        |> List.sortBy .ord


toEditingList : Dict FloorId FloorInfo -> List FloorBase
toEditingList floorsInfo =
    floorsInfo
        |> toValues
        |> List.map editingFloor
        |> List.sortWith
            (\f1 f2 ->
                if f1.temporary == f2.temporary then
                    intToOrd f1.ord f2.ord
                else if f2.temporary then
                    GT
                else
                    LT
            )


intToOrd : Int -> Int -> Order
intToOrd i1 i2 =
    if i1 > i2 then
        GT
    else if i1 < i2 then
        LT
    else
        EQ


sortByPublicOrder : Dict FloorId FloorInfo -> Maybe FloorInfo
sortByPublicOrder floorsInfo =
    floorsInfo
        |> toValues
        |> List.sortBy (\info -> publicFloor info |> Maybe.map .ord |> Maybe.withDefault 999999)
        |> List.head
