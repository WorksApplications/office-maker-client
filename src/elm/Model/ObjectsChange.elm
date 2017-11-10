module Model.ObjectsChange exposing (..)

import CoreType exposing (..)
import Dict exposing (Dict)
import Model.Object as Object exposing (..)


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


merge : ObjectsChange -> ObjectsChange -> ObjectsChange
merge new old =
    Dict.merge
        (\id new dict -> Dict.insert id new dict)
        (\id new old dict ->
            case ( new, old ) of
                ( Deleted _, Added _ ) ->
                    dict

                ( Modified { new }, Added _ ) ->
                    Dict.insert id (Added new) dict

                ( Modified n, Modified o ) ->
                    Dict.insert
                        id
                        (Modified
                            { new = n.new
                            , old = n.old

                            -- TODO dedupe
                            , changes = o.changes ++ n.changes
                            }
                        )
                        dict

                _ ->
                    Dict.insert id new dict
        )
        (\id old dict -> Dict.insert id old dict)
        new
        old
        Dict.empty


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
