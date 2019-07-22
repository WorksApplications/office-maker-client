module Page.Map.SelectedObjectsActions exposing (view)

import CoreType exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (style)
import Model.EditingFloor as EditingFloor
import Model.Floor as Floor
import Model.I18n as I18n
import Model.Object as Object
import Page.Map.Model exposing (DraggingContext(..), Model)
import Page.Map.Msg exposing (..)
import Util.HtmlUtil exposing (..)
import View.Styles as S


view : Model -> List (Html Msg)
view model =
    constructItems model
        (\label annotation msg ->
            button
                [ onClick_ msg
                , style S.formButton
                ]
                [ text label ]
        )


constructItems : Model -> (String -> Maybe String -> Msg -> result) -> List result
constructItems model makeItem =
    Maybe.withDefault []
        (List.head model.selectedObjects
            |> Maybe.map (\id -> constructItemsWithId id model makeItem)
        )


constructItemsWithId : ObjectId -> Model -> (String -> Maybe String -> Msg -> result) -> List result
constructItemsWithId id model makeItem =
    let
        itemsForPerson =
            model.floor
                |> Maybe.andThen
                    (\eFloor ->
                        Floor.getObject id (EditingFloor.present eFloor)
                            |> Maybe.andThen
                                (\obj ->
                                    Object.relatedPerson obj
                                        |> Maybe.andThen
                                            (\personId ->
                                                Dict.get personId model.personInfo
                                                    |> Maybe.map
                                                        (\person ->
                                                            [ makeItem (I18n.selectSamePost model.lang) (Just person.post) (SelectSamePost person.post)
                                                            , makeItem (I18n.searchSamePost model.lang) (Just person.post) (SearchByPost person.post)
                                                            ]
                                                        )
                                            )
                                )
                    )

        forOneDesk =
            if List.length model.selectedObjects == 1 then
                Maybe.withDefault [] itemsForPerson
                    ++ [ makeItem (I18n.selectIsland model.lang) Nothing (SelectIsland id)
                       , makeItem (I18n.selectSameColor model.lang) Nothing (SelectSameColor id)
                       , makeItem (I18n.selectSameColor model.lang) Nothing (SelectSameColor id)
                       , makeItem (I18n.registerAsStamp model.lang) Nothing (RegisterPrototype id)
                       ]

            else
                []

        common =
            [ makeItem (I18n.pickupFirstWord model.lang) Nothing (FirstNameOnly model.selectedObjects)
            , makeItem (I18n.removeSpaces model.lang) Nothing (RemoveSpaces model.selectedObjects)
            , makeItem (I18n.rotate model.lang) Nothing (RotateObjects model.selectedObjects)
            , makeItem (I18n.detachProfiles model.lang) Nothing (DetachProfiles model.selectedObjects)
            ]
    in
    forOneDesk ++ common
