module View.Icons exposing (backgroundColorPropLabel, closeButton, colorPropLabel, defaultColor, editingToggle, fontSizePropLabel, headerIconColor, helpButton, labelMode, link, mode, modeColor, penMode, personDetailPopupPersonEmployeeId, personDetailPopupPersonMail, personDetailPopupPersonTel, personMatched, personNotMatched, popupClose, printButton, proplabelColor, saveButton, searchResultClose, searchResultItemPerson, searchResultItemPost, selectMode, shapeEllipse, shapePropLabel, shapeRectangle, stampMode, userMenuToggle)

import Color exposing (Color, white)
import FontAwesome exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)


createIcon : Icon -> Color -> Int -> Html msg
createIcon icon color size =
    iconWithOptions
        icon
        Solid
        []
        [ style [ ( "color", toString color ), ( "font-size", toString size ++ "px" ) ]
        ]


defaultColor : Color
defaultColor =
    Color.rgb 140 140 140


modeColor : Color
modeColor =
    Color.rgb 90 90 90


link : Svg msg
link =
    createIcon FontAwesome.link defaultColor 16


mode : (Color -> Int -> Svg msg) -> (Bool -> Svg msg)
mode f =
    \selected ->
        f
            (if selected then
                white

             else
                modeColor
            )
            24


selectMode : Bool -> Svg msg
selectMode =
    mode (createIcon mousePointer)


penMode : Bool -> Svg msg
penMode =
    mode (createIcon pencil)


stampMode : Bool -> Svg msg
stampMode =
    mode (createIcon thLarge)


labelMode : Bool -> Svg msg
labelMode =
    mode (createIcon font)


personMatched : Float -> Svg msg
personMatched ratio =
    createIcon check white (Basics.floor (18 * ratio))


personNotMatched : Float -> Svg msg
personNotMatched ratio =
    createIcon question white (Basics.floor (18 * ratio))


popupClose : Svg msg
popupClose =
    createIcon times defaultColor 18


searchResultClose : Svg msg
searchResultClose =
    createIcon times defaultColor 18


proplabelColor : Color
proplabelColor =
    defaultColor


backgroundColorPropLabel : Svg msg
backgroundColorPropLabel =
    createIcon thLarge proplabelColor 12


colorPropLabel : Svg msg
colorPropLabel =
    createIcon font proplabelColor 12


shapePropLabel : Svg msg
shapePropLabel =
    createIcon star proplabelColor 12


fontSizePropLabel : Svg msg
fontSizePropLabel =
    createIcon font proplabelColor 12


shapeRectangle : Svg msg
shapeRectangle =
    createIcon square defaultColor 20


shapeEllipse : Svg msg
shapeEllipse =
    createIcon circle defaultColor 20


searchResultItemPerson : Svg msg
searchResultItemPerson =
    createIcon user defaultColor 20


searchResultItemPost : Svg msg
searchResultItemPost =
    createIcon user defaultColor 20


personDetailPopupPersonTel : Svg msg
personDetailPopupPersonTel =
    createIcon phone defaultColor 16


personDetailPopupPersonEmployeeId : Svg msg
personDetailPopupPersonEmployeeId =
    createIcon creditCard defaultColor 16


personDetailPopupPersonMail : Svg msg
personDetailPopupPersonMail =
    createIcon envelope defaultColor 16


headerIconColor : Color
headerIconColor =
    white


editingToggle : Svg msg
editingToggle =
    createIcon pencil headerIconColor 22


printButton : Bool -> Svg msg
printButton printMode =
    if printMode then
        createIcon print defaultColor 22

    else
        createIcon print headerIconColor 22


saveButton : Bool -> Svg msg
saveButton printMode =
    if printMode then
        createIcon download defaultColor 22

    else
        createIcon download headerIconColor 22


closeButton : Bool -> Svg msg
closeButton printMode =
    if printMode then
        createIcon times defaultColor 22

    else
        createIcon times headerIconColor 22


helpButton : Svg msg
helpButton =
    createIcon questionCircle headerIconColor 22


userMenuToggle : Bool -> Svg msg
userMenuToggle open =
    createIcon
        (if open then
            caretUp

         else
            caretDown
        )
        white
        16
