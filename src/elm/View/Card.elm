module View.Card exposing
    ( card
    , foldableCard
    , formControl
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.I18n as I18n exposing (Language)
import Util.StyleUtil exposing (..)
import View.CommonStyles exposing (..)
import View.Icons as Icons


foldableCard : Bool -> Language -> msg -> Bool -> String -> Maybe Int -> Maybe Int -> List (Html msg) -> Html msg
foldableCard folded lang toggleMsg absolute backgroundColor maxHeight maybeWidth children =
    card absolute backgroundColor maxHeight maybeWidth <|
        (if List.isEmpty children then
            []

         else
            [ div
                [ onClick toggleMsg ]
                [ span [ style [ ( "vertical-align", "middle" ) ] ] [ Icons.accordionPanelToggle folded ]
                , text (I18n.setAccordion lang folded)
                ]
            ]
        )
            ++ (if folded then
                    []

                else
                    [ div [ style [ ( "margin-top", "0.5em" ) ] ] children ]
               )


card : Bool -> String -> Maybe Int -> Maybe Int -> List (Html msg) -> Html msg
card absolute backgroundColor maxHeight maybeWidth children =
    div
        [ style (cardStyles absolute backgroundColor maxHeight maybeWidth) ]
        children


cardStyles : Bool -> String -> Maybe Int -> Maybe Int -> S
cardStyles absolute backgroundColor maybeMaxHeight maybeWidth =
    (case maybeMaxHeight of
        Just maxHeight ->
            [ ( "max-height", px maxHeight )
            , ( "overflow-y", "scroll" )
            , ( "box-sizing", "border-box" )
            ]

        Nothing ->
            []
    )
        ++ [ ( "background-color", backgroundColor )
           , ( "width", maybeWidth |> Maybe.map px |> Maybe.withDefault "" )
           , ( "position"
             , if absolute then
                "absolute"

               else
                ""
             )
           , ( "z-index"
             , if absolute then
                "1"

               else
                ""
             )
           ]
        ++ View.CommonStyles.card


formControl : List (Html msg) -> Html msg
formControl children =
    div [ style View.CommonStyles.formControl ] children
