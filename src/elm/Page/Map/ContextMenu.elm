module Page.Map.ContextMenu exposing (view)

import ContextMenu
import Html exposing (..)
import Model.EditingFloor as EditingFloor
import Model.I18n as I18n
import Model.User as User
import Page.Map.ContextMenuContext exposing (..)
import Page.Map.Model exposing (DraggingContext(..), Model)
import Page.Map.Msg exposing (..)


view : Model -> Html Msg
view model =
    ContextMenu.view
        ContextMenu.defaultConfig
        ContextMenuMsg
        (toItemGroups model)
        model.contextMenu


toItemGroups : Model -> ContextMenuContext -> List (List ( ContextMenu.Item, Msg ))
toItemGroups model context =
    case context of
        FloorInfoContextMenu floorId ->
            if Maybe.map (EditingFloor.present >> .id) model.floor == Just floorId then
                if User.isGuest model.user then
                    []

                else if User.isAdmin model.user then
                    [ [ ( ContextMenu.item (I18n.copyFloor model.lang), CopyFloor floorId False )
                      , ( ContextMenu.item (I18n.copyAndCreateTemporaryFloor model.lang), CopyFloor floorId True )
                      ]
                    ]

                else
                    [ [ ( ContextMenu.item (I18n.copyAndCreateTemporaryFloor model.lang), CopyFloor floorId True )
                      ]
                    ]

            else
                []
