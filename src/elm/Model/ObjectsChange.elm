module Model.ObjectsChange exposing (..)

import Dict exposing (Dict)
import Model.Object as Object exposing (..)
import CoreType exposing (..)


type alias ObjectModification =
    { new : Object, old : Object, changes : List ObjectPropertyChange }


type ObjectChange
    = Added Object
    | Modified ObjectModification
    | Deleted Object


type alias ObjectsChange =
    Dict ObjectId ObjectChange


added : List Object -> ObjectsChange
added objects =
    objects
        |> List.map (\object -> ( Object.idOf object, Added object ))
        |> Dict.fromList


modified : List ( ObjectId, ObjectModification ) -> ObjectsChange
modified idRelatedList =
    idRelatedList
        |> List.map (\( id, a ) -> ( id, Modified a ))
        |> Dict.fromList


empty : ObjectsChange
empty =
    Dict.empty


isEmpty : ObjectsChange -> Bool
isEmpty change =
    Dict.isEmpty change


merge : Dict ObjectId Object -> ObjectsChange -> ObjectsChange -> ObjectsChange
merge currentObjects new old =
    Dict.merge
        (\id new dict -> insertToMergedDict currentObjects id new dict)
        (\id new old dict ->
            case ( new, old ) of
                ( Deleted _, Added _ ) ->
                    dict

                ( Modified { new }, Added _ ) ->
                    insertToMergedDict currentObjects id (Added new) dict

                ( Modified n, Modified o ) ->
                    insertToMergedDict currentObjects
                        id
                        (Modified
                            { new = n.new
                            , old = n.old

                            -- TODO dedupe
                            , changes = n.changes ++ o.changes
                            }
                        )
                        dict

                _ ->
                    insertToMergedDict currentObjects id new dict
        )
        (\id old dict -> insertToMergedDict currentObjects id old dict)
        new
        old
        Dict.empty


insertToMergedDict : Dict ObjectId Object -> ObjectId -> ObjectChange -> ObjectsChange -> ObjectsChange
insertToMergedDict currentObjects id value dict =
    currentObjects
        |> Dict.get id
        |> Maybe.map
            (\currentObject ->
                Dict.insert id (copyCurrentUpdateAtToObjects currentObject value) dict
            )
        |> Maybe.withDefault (Dict.insert id value dict)


{-| current object does not exist if deleted
-}
copyCurrentUpdateAtToObjects : Object -> ObjectChange -> ObjectChange
copyCurrentUpdateAtToObjects currentObject modification =
    case modification of
        Added object ->
            Added (Object.copyUpdateAt currentObject object)

        Modified { old, new, changes } ->
            Modified { old = old, new = Object.copyUpdateAt currentObject new, changes = changes }

        Deleted object ->
            Deleted (Object.copyUpdateAt currentObject object)


fromList : List ( ObjectId, ObjectChange ) -> ObjectsChange
fromList list =
    Dict.fromList list


toList : ObjectsChange -> List ObjectChange
toList change =
    List.map Tuple.second (Dict.toList change)


separate : ObjectsChange -> { added : List Object, modified : List ObjectModification, deleted : List Object }
separate change =
    let
        ( added, modified, deleted ) =
            Dict.foldl
                (\_ value ( added, modified, deleted ) ->
                    case value of
                        Added object ->
                            ( object :: added, modified, deleted )

                        Modified a ->
                            ( added, a :: modified, deleted )

                        Deleted object ->
                            ( added, modified, object :: deleted )
                )
                ( [], [], [] )
                change
    in
        { added = added
        , modified = modified
        , deleted = deleted
        }
