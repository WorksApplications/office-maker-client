module Page.Map.PrintGuide exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import InlineHover exposing (hover)
import Model.I18n as I18n exposing (Language)
import Page.Map.Msg exposing (Msg(..))
import Util.StyleUtil exposing (..)
import View.Styles as Styles


view : Language -> Bool -> Html Msg
view lang isPrintMode =
    if isPrintMode then
        div
            [ style containerStyle
            , class "no-print"
            ]
            [ div [ style (itemStyle 2245 1587) ] [ text "A2" ]
            , div [ style (itemStyle 1587 1122) ] [ text "A3" ]
            , div [ style (itemStyle 1122 793) ]
                [ div [] [ text "A4" ]
                , div
                    [ style
                        [ ( "display", "flex" )
                        , ( "position", "static" )
                        , ( "float", "right" )
                        ]
                    ]
                    [ lazy printButton lang, lazy saveButton lang ]
                ]
            ]
    else
        text ""


printButton : Language -> Html Msg
printButton lang =
    hover Styles.printModeHover
        div
        [ style buttonStyle
        , onClick Print
        ]
        [ text (I18n.print lang) ]


saveButton : Language -> Html Msg
saveButton lang =
    hover Styles.printModeHover
        div
        [ style buttonStyle
        , onClick Print

        -- , onClick Save
        ]
        [ text (I18n.save lang) ]


color : String
color =
    "rgb(200, 150, 220)"


buttonColor : String
buttonColor =
    "rgb(200, 0, 220)"


containerStyle : List ( String, String )
containerStyle =
    [ ( "position", "fixed" )
    , ( "z-index", Styles.zPrintGuide )
    , ( "top", "0" )
    , ( "left", "0" )
    , ( "pointer-events", "none" )
    ]


itemStyle : Int -> Int -> List ( String, String )
itemStyle width height =
    [ ( "position", "fixed" )
    , ( "top", "0" )
    , ( "left", "0" )
    , ( "width", px width )
    , ( "height", px height )
    , ( "border", "dashed 5px " ++ color )
    , ( "font-size", "x-large" )
    , ( "font-weight", "bold" )
    , ( "color", color )
    , ( "text-align", "right" )
    , ( "padding-right", "3px" )
    ]


buttonStyle : List ( String, String )
buttonStyle =
    [ ( "border", "2px solid " ++ color )
    , ( "color", "white" )
    , ( "position", "static" )
    , ( "top", "0" )
    , ( "bottom", "0" )
    , ( "left", "0" )
    , ( "right", "0" )
    , ( "width", "100px" )
    , ( "height", "50px" )
    , ( "line-height", "50px" )
    , ( "text-align", "center" )
    , ( "font-size", "1em" )
    , ( "font-weight", "normal" )
    , ( "cursor", "pointer" )
    , ( "background-color", buttonColor )
    , ( "opacity", "0.4" )
    , ( "pointer-events", "all" )
    , ( "margin-right", "5px" )
    ]
