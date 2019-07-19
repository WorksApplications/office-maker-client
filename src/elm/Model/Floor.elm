module Model.Floor exposing
    ( Detailed
    , Floor
    , FloorBase
    , ObjectDiff
    , addObjects
    , baseOf
    , changeId
    , changeName
    , changeObjectBackgroundColor
    , changeObjectColor
    , changeObjectFontSize
    , changeObjectName
    , changeObjectShape
    , changeObjectUrl
    , changeObjectsByChanges
    , changeOrd
    , changeRealSize
    , copy
    , decodeObjectDiff
    , empty
    , filterObjectsInFloor
    , flip
    , fullyChangeObjects
    , getObject
    , getObjects
    , height
    , init
    , initWithOrder
    , modifyObjects
    , move
    , moveObjects
    , name
    , objects
    , objectsDictFromList
    , partiallyChangeObjects
    , partiallyChangeRelatedPersons
    , paste
    , patchObjectsByDiffs
    , pixelToReal
    , realSize
    , realToPixel
    , removeObjects
    , removeSpaces
    , resizeObject
    , rotateObjects
    , setImage
    , setObjects
    , setPeople
    , setPerson
    , size
    , src
    , toFirstNameOnly
    , unsetPerson
    , whiteSpaces
    , width
    )

import CoreType exposing (..)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as D
import Model.Floor.ObjectDiff as ObjectDiff exposing (ObjectDiff)
import Model.Object as Object exposing (Object)
import Model.ObjectsChange as ObjectsChange exposing (ObjectModification, ObjectsChange)
import Model.ObjectsOperation as ObjectsOperation
import Regex exposing (Regex)



-- Re-exports of ObjectDiff


type alias ObjectDiff =
    ObjectDiff.ObjectDiff


decodeObjectDiff : D.Decoder ObjectDiff
decodeObjectDiff =
    ObjectDiff.decoder



--


type alias FloorBase =
    { id : FloorId
    , temporary : Bool
    , name : String
    , ord : Int
    , update : Maybe { by : PersonId, at : Date }
    }


type alias Detailed a =
    { a
        | width : Int
        , height : Int
        , realSize : Maybe ( Int, Int )
        , image : Maybe String
        , flipImage : Bool
        , objects : Dict ObjectId Object
    }


type alias Floor =
    Detailed FloorBase


init : FloorId -> Floor
init id =
    { id = id
    , name = "New Floor"
    , ord = 0
    , objects = Dict.empty
    , width = 7200
    , height = 4560
    , realSize = Just ( 10, 7 )
    , temporary = False
    , image = Nothing
    , flipImage = False
    , update = Nothing
    }


empty : Floor
empty =
    init ""


baseOf : Floor -> FloorBase
baseOf { id, temporary, name, ord, update } =
    FloorBase id temporary name ord update


initWithOrder : FloorId -> Int -> Floor
initWithOrder id ord =
    let
        floor =
            init id
    in
    { floor
        | ord = ord
    }


changeName : String -> Floor -> Floor
changeName name floor =
    { floor | name = name }


changeOrd : Int -> Floor -> Floor
changeOrd ord floor =
    { floor | ord = ord }


setImage : String -> Int -> Int -> Floor -> Floor
setImage url width height floor =
    { floor
        | width = width
        , height = height
        , image = Just url
    }


changeRealSize : ( Int, Int ) -> Floor -> Floor
changeRealSize ( width, height ) floor =
    { floor
        | realSize = Just ( width, height )
    }



{- 10cm -> 8px -}


realToPixel : Int -> Int
realToPixel real =
    Basics.floor (toFloat real * 80)


pixelToReal : Int -> Int
pixelToReal pixel =
    Basics.floor (toFloat pixel / 80)


size : Floor -> Size
size floor =
    case floor.realSize of
        Just ( w, h ) ->
            Size (realToPixel w) (realToPixel h)

        Nothing ->
            Size floor.width floor.height


name : Floor -> String
name floor =
    floor.name


width : Floor -> Int
width floor =
    size floor |> .width


height : Floor -> Int
height floor =
    size floor |> .height



-- TODO confusing...


realSize : Floor -> ( Int, Int )
realSize floor =
    case floor.realSize of
        Just ( w, h ) ->
            ( w, h )

        Nothing ->
            ( pixelToReal floor.width, pixelToReal floor.height )


src : String -> Floor -> Maybe String
src imageRoot floor =
    case floor.image of
        Just src ->
            Just (imageRoot ++ "/floors/" ++ src)

        Nothing ->
            Nothing


changeId : FloorId -> Floor -> Floor
changeId id floor =
    { floor | id = id }


copy : FloorId -> Bool -> Floor -> Floor
copy id temporary floor =
    { floor
        | id = id
        , name =
            if temporary then
                "Temporary from " ++ floor.name

            else
                "Copy of " ++ floor.name
        , update = Nothing
        , objects = Dict.empty
        , temporary = temporary
    }


flip : Floor -> Floor
flip floor =
    { floor
        | flipImage = not floor.flipImage
    }
        |> fullyChangeObjects (ObjectsOperation.flipObject <| size floor)



-- OBJECT OPERATIONS


move : List ObjectId -> Int -> ( Int, Int ) -> Floor -> Floor
move ids gridSize ( dx, dy ) floor =
    partiallyChangeObjects
        (moveObjects gridSize ( dx, dy ))
        ids
        floor


moveObjects : Int -> ( Int, Int ) -> Object -> Object
moveObjects gridSize ( dx, dy ) object =
    let
        pos =
            Object.positionOf object

        new =
            ObjectsOperation.fitPositionToGrid
                gridSize
                (Position (pos.x + dx) (pos.y + dy))
    in
    Object.changePosition new object


paste : List ( Object, ObjectId ) -> Position -> Floor -> Floor
paste copiedWithNewIds base floor =
    addObjects
        (ObjectsOperation.pasteObjects floor.id base copiedWithNewIds)
        floor


rotateObjects : List ObjectId -> Floor -> Floor
rotateObjects ids floor =
    partiallyChangeObjects Object.rotate ids floor


changeObjectColor : List ObjectId -> String -> Floor -> Floor
changeObjectColor ids color floor =
    partiallyChangeObjects (Object.changeColor color) ids floor


changeObjectBackgroundColor : List ObjectId -> String -> Floor -> Floor
changeObjectBackgroundColor ids color floor =
    partiallyChangeObjects (Object.changeBackgroundColor color) ids floor


changeObjectShape : List ObjectId -> Object.Shape -> Floor -> Floor
changeObjectShape ids shape floor =
    partiallyChangeObjects (Object.changeShape shape) ids floor


changeObjectName : List ObjectId -> String -> Floor -> Floor
changeObjectName ids name floor =
    partiallyChangeObjects (Object.changeName name) ids floor


changeObjectFontSize : List ObjectId -> Float -> Floor -> Floor
changeObjectFontSize ids fontSize floor =
    partiallyChangeObjects (Object.changeFontSize fontSize) ids floor


changeObjectUrl : List ObjectId -> String -> Floor -> Floor
changeObjectUrl ids url floor =
    partiallyChangeObjects (Object.changeUrl url) ids floor


changeObjectsByChanges : ObjectsChange -> Floor -> Floor
changeObjectsByChanges change floor =
    let
        separated =
            ObjectsChange.separate change
    in
    floor
        |> addObjects separated.added
        |> modifyObjects separated.modified
        |> removeObjects (List.map Object.idOf separated.deleted)


toFirstNameOnly : List ObjectId -> Floor -> Floor
toFirstNameOnly ids floor =
    let
        change name =
            case String.words name of
                [] ->
                    ""

                x :: _ ->
                    x

        f object =
            Object.changeName (change (Object.nameOf object)) object
    in
    partiallyChangeObjects f ids floor


fullyChangeObjects : (Object -> Object) -> Floor -> Floor
fullyChangeObjects f floor =
    { floor
        | objects =
            Dict.map (\_ object -> f object) floor.objects
    }


partiallyChangeObjects : (Object -> Object) -> List ObjectId -> Floor -> Floor
partiallyChangeObjects f ids floor =
    { floor
        | objects =
            ids
                |> List.foldl
                    (\objectId dict -> Dict.update objectId (Maybe.map f) dict)
                    floor.objects
    }


{-| Similar to `partiallyChangeObjects`, but only act on the objects personId attached.
-}
partiallyChangeRelatedPersons : (PersonId -> Object -> Object) -> List ObjectId -> Floor -> Floor
partiallyChangeRelatedPersons f ids floor =
    { floor
        | objects =
            ids
                |> List.foldl
                    (\objectId dict ->
                        Dict.update objectId
                            (\mayObject ->
                                mayObject
                                    |> Maybe.map
                                        (\object ->
                                            Object.relatedPerson object
                                                |> Maybe.map (\personId -> f personId object)
                                                |> Maybe.withDefault object
                                        )
                            )
                            dict
                    )
                    floor.objects
    }


removeSpaces : List ObjectId -> Floor -> Floor
removeSpaces ids floor =
    let
        change name =
            Regex.replace Regex.All whiteSpaces (\_ -> "") name

        f object =
            Object.changeName (change <| Object.nameOf object) object
    in
    partiallyChangeObjects f ids floor


whiteSpaces : Regex
whiteSpaces =
    Regex.regex "[ \\x0D\n\\x3000]"


resizeObject : ObjectId -> Size -> Floor -> Floor
resizeObject id size floor =
    partiallyChangeObjects (Object.changeSize size) [ id ] floor


setPerson : ObjectId -> PersonId -> Floor -> Floor
setPerson objectId personId floor =
    setPeople [ ( objectId, personId ) ] floor


unsetPerson : ObjectId -> Floor -> Floor
unsetPerson objectId floor =
    partiallyChangeObjects (Object.setPerson Nothing) [ objectId ] floor


setPeople : List ( ObjectId, PersonId ) -> Floor -> Floor
setPeople pairs floor =
    let
        f ( objectId, personId ) dict =
            dict
                |> Dict.update objectId (Maybe.map (Object.setPerson (Just personId)))

        newObjects =
            List.foldl f floor.objects pairs
    in
    { floor | objects = newObjects }


objects : Floor -> List Object
objects floor =
    Dict.values floor.objects


getObject : ObjectId -> Floor -> Maybe Object
getObject objectId floor =
    Dict.get objectId floor.objects


getObjects : List ObjectId -> Floor -> List Object
getObjects ids floor =
    ids
        |> List.filterMap (\id -> getObject id floor)


setObjects : List Object -> Floor -> Floor
setObjects objects floor =
    { floor
        | objects =
            objectsDictFromList floor.id objects
    }


{-| bad name!!
-}
addObjects : List Object -> Floor -> Floor
addObjects objects floor =
    { floor
        | objects =
            objects
                |> filterObjectsInFloor floor.id
                |> List.foldl (\object -> Dict.insert (Object.idOf object) object) floor.objects
    }


modifyObjects : List ObjectModification -> Floor -> Floor
modifyObjects list floor =
    { floor
        | objects =
            list
                |> List.foldl
                    (\mod dict ->
                        Dict.update
                            (Object.idOf mod.new)
                            (Maybe.map (Object.modifyAll mod.changes))
                            dict
                    )
                    floor.objects
    }


removeObjects : List ObjectId -> Floor -> Floor
removeObjects objectIds floor =
    { floor
        | objects =
            List.foldl Dict.remove floor.objects objectIds
    }


objectsDictFromList : FloorId -> List Object -> Dict ObjectId Object
objectsDictFromList floorId objects =
    objects
        |> filterObjectsInFloor floorId
        |> List.map (\object -> ( Object.idOf object, object ))
        |> Dict.fromList


filterObjectsInFloor : FloorId -> List Object -> List Object
filterObjectsInFloor floorId objects =
    objects
        |> List.filter (\object -> Object.floorIdOf object == floorId)


{-| Patch objects using ObjectDiff list
-}
patchObjectsByDiffs : Object.ObjectDecoderDefault -> List ObjectDiff -> Floor -> Floor
patchObjectsByDiffs def diffs floor =
    let
        collected =
            ObjectDiff.collect diffs

        objectDict =
            floor.objects

        maybeOr x y =
            case x of
                Just xval ->
                    Just xval

                Nothing ->
                    case y of
                        Just yval ->
                            Just yval

                        Nothing ->
                            Nothing

        {- This function will construct an "updated" object from ObjectDiffProps.
           It is needed since the received data from subscription is only the difference,
           not the whole object itself. But to update the floor object, we need to convert it to
           a complete (not partial) object. To make things worse, ObjectDiffProps has flatten structure,
           which is different from ObjectChange (ObjectChange has its own structure) so we cannot just
           apply the diff to the existing object. Here is the part to recover the structure and
           patch the diff to the existing one.
        -}
        patchObject : Object -> ObjectDiff.ObjectDiffProps -> Object
        patchObject object diff =
            let
                oldPosition =
                    Object.positionOf object

                newPosition =
                    { oldPosition
                        | x = Maybe.withDefault oldPosition.x (ObjectDiff.at diff "x" D.int)
                        , y = Maybe.withDefault oldPosition.y (ObjectDiff.at diff "y" D.int)
                    }

                oldSize =
                    Object.sizeOf object

                newSize =
                    { oldSize
                        | width = Maybe.withDefault oldSize.width (ObjectDiff.at diff "width" D.int)
                        , height = Maybe.withDefault oldSize.height (ObjectDiff.at diff "height" D.int)
                    }

                oldExtension =
                    case object of
                        Object.Object object ->
                            object.extension

                newExtension =
                    (ObjectDiff.at diff "personId" D.string
                        |> Maybe.map (\personId -> Object.Desk (Just personId))
                    )
                        |> maybeOr
                            (Maybe.map4 (\color bold url shape -> Object.Label { color = color, bold = bold, url = url, shape = shape })
                                (maybeOr (Object.mayDeskColorOf object) (ObjectDiff.at diff "color" D.string))
                                (maybeOr (Object.mayDeskBoldOf object) (ObjectDiff.at diff "bold" D.bool))
                                (maybeOr (Object.mayDeskColorOf object) (ObjectDiff.at diff "url" D.string))
                                (maybeOr (Object.mayDeskShapeOf object)
                                    (ObjectDiff.at diff "shape" D.string
                                        |> Maybe.map
                                            (\shape ->
                                                if shape == "rectangle" then
                                                    Object.Rectangle

                                                else
                                                    Object.Ellipse
                                            )
                                    )
                                )
                            )
                        |> Maybe.withDefault oldExtension
            in
            Object.Object
                { id = Object.idOf object
                , floorId = Object.floorIdOf object
                , position = newPosition
                , size = newSize
                , backgroundColor = Maybe.withDefault (Object.backgroundColorOf object) <| ObjectDiff.at diff "backgroundColor" D.string
                , name = Maybe.withDefault (Object.nameOf object) <| ObjectDiff.at diff "name" D.string
                , fontSize = Maybe.withDefault (Object.fontSizeOf object) <| ObjectDiff.at diff "fontSize" D.float
                , extension = newExtension
                , updateAt = Object.updateAtOf object
                }
    in
    floor
        |> addObjects
            (collected.added
                |> List.filterMap
                    (\props ->
                        ObjectDiff.extractJson props
                            |> D.decodeValue (Object.decodeObjectWithDefault def)
                            |> Result.toMaybe
                    )
            )
        -- Since we constructed the whole (complete) object data, we only need to insert/override them
        -- `addObjects` works as `putObjects`
        |> addObjects
            (collected.modified
                |> List.filterMap
                    (\props ->
                        Dict.get props.id objectDict
                            |> Maybe.map (\targetObject -> patchObject targetObject props)
                    )
            )
        |> removeObjects (collected.deleted |> List.map (\props -> props.id))
