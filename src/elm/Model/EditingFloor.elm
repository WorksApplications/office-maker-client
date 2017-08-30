module Model.EditingFloor exposing (..)

import Model.Floor as Floor exposing (Floor)
import Model.FloorDiff as FloorDiff
import Model.ObjectsChange as ObjectsChange exposing (ObjectsChange)
import Util.UndoList as UndoList exposing (UndoList)


type alias EditingFloor =
    { undoList : UndoList Floor
    }


init : Floor -> EditingFloor
init floor =
    { undoList = UndoList.init floor
    }


updateFloorAndObjects : (Floor -> Floor) -> EditingFloor -> ( EditingFloor, Floor, ObjectsChange )
updateFloorAndObjects f efloor =
    let
        floor =
            efloor.undoList.present

        newFloor =
            f floor

        propChanged =
            FloorDiff.diffPropertyChanges newFloor (Just floor)

        objectsChange =
            FloorDiff.diffObjects newFloor.objects floor.objects

        changed =
            propChanged /= [] || (not <| ObjectsChange.isEmpty objectsChange)

        newUndoList =
            if changed then
                UndoList.new newFloor efloor.undoList
            else
                efloor.undoList
    in
        ( { efloor | undoList = newUndoList }
        , newFloor
        , objectsChange
        )


updateFloor : (Floor -> Floor) -> EditingFloor -> ( EditingFloor, Floor )
updateFloor f efloor =
    let
        floor =
            efloor.undoList.present

        newFloor =
            f floor

        propChanged =
            FloorDiff.diffPropertyChanges newFloor (Just floor)

        changed =
            propChanged /= []

        newUndoList =
            if changed then
                UndoList.new newFloor efloor.undoList
            else
                efloor.undoList
    in
        ( { efloor | undoList = newUndoList }
        , newFloor
        )


updateObjects : (Floor -> Floor) -> EditingFloor -> ( EditingFloor, ObjectsChange )
updateObjects f efloor =
    let
        floor =
            efloor.undoList.present

        newFloor =
            f floor

        objectsChange =
            FloorDiff.diffObjects newFloor.objects floor.objects

        changed =
            not <| ObjectsChange.isEmpty objectsChange

        newUndoList =
            if changed then
                UndoList.new newFloor efloor.undoList
            else
                efloor.undoList
    in
        ( { efloor | undoList = newUndoList }, objectsChange )


undo : EditingFloor -> ( EditingFloor, ObjectsChange )
undo efloor =
    let
        ( undoList, maybeObjectsChange ) =
            UndoList.undoWithDiff
                (\prev current ->
                    FloorDiff.diffObjects prev.objects current.objects
                )
                efloor.undoList
    in
        case maybeObjectsChange of
            Just objectsChange ->
                ( { efloor | undoList = undoList }, objectsChange )

            Nothing ->
                ( efloor, ObjectsChange.empty )


redo : EditingFloor -> ( EditingFloor, ObjectsChange )
redo efloor =
    let
        ( undoList, maybeObjectsChange ) =
            UndoList.redoWithDiff
                (\next current ->
                    FloorDiff.diffObjects next.objects current.objects
                )
                efloor.undoList
    in
        case maybeObjectsChange of
            Just objectsChange ->
                ( { efloor | undoList = undoList }, objectsChange )

            Nothing ->
                ( efloor, ObjectsChange.empty )


present : EditingFloor -> Floor
present efloor =
    efloor.undoList.present
