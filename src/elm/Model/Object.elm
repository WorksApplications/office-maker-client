module Model.Object exposing
    ( LabelFields
    , Object(..)
    , ObjectDecoderDefault
    , ObjectExtension(..)
    , ObjectPropertyChange(..)
    , Shape(..)
    , backgroundColorEditable
    , backgroundColorOf
    , bottom
    , changeBackgroundColor
    , changeBold
    , changeColor
    , changeFloorId
    , changeFontSize
    , changeId
    , changeName
    , changePosition
    , changeShape
    , changeSize
    , changeUrl
    , colorEditable
    , colorOf
    , decodeObjectWithDefault
    , defaultFontSize
    , floorIdOf
    , fontSizeEditable
    , fontSizeOf
    , futureForInitialUpdateTime
    , idOf
    , initDesk
    , initLabel
    , isBold
    , isDesk
    , isLabel
    , left
    , mayDeskBoldOf
    , mayDeskColorOf
    , mayDeskShapeOf
    , mayDeskUrlOf
    , modify
    , modifyAll
    , nameOf
    , positionOf
    , relatedPerson
    , right
    , rotate
    , setPerson
    , shapeEditable
    , shapeOf
    , sizeOf
    , top
    , updateAtOf
    , urlEditable
    , urlOf
    )

import CoreType exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Time exposing (Time)
import Util.DecodeUtil exposing (optional_)


type Shape
    = Rectangle
    | Ellipse


type Object
    = Object
        { id : ObjectId
        , floorId : FloorId
        , position : Position
        , size : Size
        , backgroundColor : String
        , name : String
        , fontSize : Float
        , extension : ObjectExtension
        , updateAt : Float
        }


type alias LabelFields =
    { color : String
    , bold : Bool
    , url : String
    , shape : Shape
    }


type ObjectExtension
    = Desk (Maybe PersonId)
    | Label LabelFields


type ObjectPropertyChange
    = ChangeName String String
    | ChangeSize Size Size
    | ChangePosition Position Position
    | ChangeBackgroundColor String String
    | ChangeColor String String
    | ChangeFontSize Float Float
    | ChangeBold Bool Bool
    | ChangeUrl String String
    | ChangeShape Shape Shape
    | ChangePerson (Maybe PersonId) (Maybe PersonId)


modifyAll : List ObjectPropertyChange -> Object -> Object
modifyAll changes object =
    changes
        |> List.foldl modify object


modify : ObjectPropertyChange -> Object -> Object
modify change object =
    case change of
        ChangeName new old ->
            changeName new object

        ChangeSize new old ->
            changeSize new object

        ChangePosition new old ->
            changePosition new object

        ChangeBackgroundColor new old ->
            changeBackgroundColor new object

        ChangeColor new old ->
            changeColor new object

        ChangeFontSize new old ->
            changeFontSize new object

        ChangeBold new old ->
            changeBold new object

        ChangeUrl new old ->
            changeUrl new object

        ChangeShape new old ->
            changeShape new object

        ChangePerson new old ->
            setPerson new object


isDesk : Object -> Bool
isDesk (Object object) =
    case object.extension of
        Desk _ ->
            True

        _ ->
            False


isLabel : Object -> Bool
isLabel (Object object) =
    case object.extension of
        Label _ ->
            True

        _ ->
            False


futureForInitialUpdateTime : Time
futureForInitialUpdateTime =
    32503647600


initDesk : ObjectId -> FloorId -> Position -> Size -> String -> String -> Float -> Maybe PersonId -> Maybe Time -> Object
initDesk id floorId position size backgroundColor name fontSize personId updateAt =
    Object
        { id = id
        , floorId = floorId
        , position = position
        , size = size
        , backgroundColor = backgroundColor
        , name = name
        , fontSize = fontSize
        , extension = Desk personId
        , updateAt = Maybe.withDefault futureForInitialUpdateTime updateAt
        }


initLabel : ObjectId -> FloorId -> Position -> Size -> String -> String -> Float -> LabelFields -> Maybe Time -> Object
initLabel id floorId position size backgroundColor name fontSize extension updateAt =
    Object
        { id = id
        , floorId = floorId
        , position = position
        , size = size
        , backgroundColor = backgroundColor
        , name = name
        , fontSize = fontSize
        , extension = Label extension
        , updateAt = Maybe.withDefault futureForInitialUpdateTime updateAt
        }


changeId : ObjectId -> Object -> Object
changeId id (Object object) =
    Object { object | id = id }


changeFloorId : FloorId -> Object -> Object
changeFloorId floorId (Object object) =
    Object { object | floorId = floorId }


changeBackgroundColor : String -> Object -> Object
changeBackgroundColor backgroundColor (Object object) =
    Object { object | backgroundColor = backgroundColor }


changeColor : String -> Object -> Object
changeColor color (Object object) =
    case object.extension of
        Desk _ ->
            Object object

        Label additionalFields ->
            Object { object | extension = Label { additionalFields | color = color } }


changeName : String -> Object -> Object
changeName name (Object object) =
    Object { object | name = name }


changeSize : Size -> Object -> Object
changeSize size (Object object) =
    Object { object | size = size }


changePosition : Position -> Object -> Object
changePosition position (Object object) =
    Object { object | position = position }


rotate : Object -> Object
rotate (Object object) =
    Object { object | size = Size object.size.height object.size.width }


setPerson : Maybe PersonId -> Object -> Object
setPerson personId ((Object object) as obj) =
    case object.extension of
        Desk _ ->
            Object { object | extension = Desk personId }

        _ ->
            obj


changeFontSize : Float -> Object -> Object
changeFontSize fontSize (Object object) =
    Object { object | fontSize = fontSize }


changeBold : Bool -> Object -> Object
changeBold bold ((Object object) as obj) =
    case object.extension of
        Desk _ ->
            obj

        Label fields ->
            Object { object | extension = Label { fields | bold = bold } }


changeUrl : String -> Object -> Object
changeUrl url ((Object object) as obj) =
    case object.extension of
        Desk _ ->
            obj

        Label fields ->
            Object { object | extension = Label { fields | url = url } }


changeShape : Shape -> Object -> Object
changeShape shape ((Object object) as obj) =
    case object.extension of
        Desk _ ->
            obj

        Label additionalFields ->
            Object { object | extension = Label { additionalFields | shape = shape } }


idOf : Object -> ObjectId
idOf (Object object) =
    object.id


floorIdOf : Object -> FloorId
floorIdOf (Object object) =
    object.floorId


nameOf : Object -> String
nameOf (Object object) =
    object.name


backgroundColorOf : Object -> String
backgroundColorOf (Object object) =
    object.backgroundColor


colorOf : Object -> String
colorOf (Object object) =
    case object.extension of
        Desk _ ->
            "#000"

        Label { color } ->
            color


mayDeskColorOf : Object -> Maybe String
mayDeskColorOf (Object object) =
    case object.extension of
        Desk _ ->
            Nothing

        Label { color } ->
            Just color


defaultFontSize : Float
defaultFontSize =
    20


fontSizeOf : Object -> Float
fontSizeOf (Object object) =
    object.fontSize


updateAtOf : Object -> Float
updateAtOf (Object object) =
    object.updateAt


isBold : Object -> Bool
isBold (Object object) =
    case object.extension of
        Desk _ ->
            False

        Label { bold } ->
            bold


mayDeskBoldOf : Object -> Maybe Bool
mayDeskBoldOf (Object object) =
    case object.extension of
        Desk _ ->
            Nothing

        Label { bold } ->
            Just bold


urlOf : Object -> String
urlOf (Object object) =
    case object.extension of
        Desk _ ->
            ""

        Label { url } ->
            url


mayDeskUrlOf : Object -> Maybe String
mayDeskUrlOf (Object object) =
    case object.extension of
        Desk _ ->
            Nothing

        Label { url } ->
            Just url


shapeOf : Object -> Shape
shapeOf (Object object) =
    case object.extension of
        Desk _ ->
            Rectangle

        Label { shape } ->
            shape


mayDeskShapeOf : Object -> Maybe Shape
mayDeskShapeOf (Object object) =
    case object.extension of
        Desk _ ->
            Nothing

        Label { shape } ->
            Just shape


sizeOf : Object -> Size
sizeOf (Object object) =
    object.size


positionOf : Object -> Position
positionOf (Object object) =
    object.position


left : Object -> Int
left object =
    .x <| positionOf object


top : Object -> Int
top object =
    .y <| positionOf object


right : Object -> Int
right (Object object) =
    object.position.x + object.size.width


bottom : Object -> Int
bottom (Object object) =
    object.position.y + object.size.height


relatedPerson : Object -> Maybe PersonId
relatedPerson (Object object) =
    case object.extension of
        Desk personId ->
            personId

        _ ->
            Nothing


backgroundColorEditable : Object -> Bool
backgroundColorEditable _ =
    True


colorEditable : Object -> Bool
colorEditable =
    isLabel


shapeEditable : Object -> Bool
shapeEditable =
    isLabel


fontSizeEditable : Object -> Bool
fontSizeEditable _ =
    True


urlEditable : Object -> Bool
urlEditable =
    isLabel



--


type alias ObjectDecoderDefault =
    { color : String, bold : Bool }


{-| Decoder for Object with given default values
-}
decodeObjectWithDefault : ObjectDecoderDefault -> Decoder Object
decodeObjectWithDefault def =
    decode
        (\id floorId tipe x y width height backgroundColor name personId fontSize color bold url shape updateAt ->
            if tipe == "desk" then
                initDesk id floorId (Position x y) (Size width height) backgroundColor name fontSize personId updateAt

            else
                initLabel id
                    floorId
                    (Position x y)
                    (Size width height)
                    backgroundColor
                    name
                    fontSize
                    (LabelFields color
                        bold
                        url
                        (if shape == "rectangle" then
                            Rectangle

                         else
                            Ellipse
                        )
                    )
                    updateAt
        )
        |> required "id" D.string
        |> required "floorId" D.string
        |> optional "type" D.string "desk"
        |> required "x" D.int
        |> required "y" D.int
        -- TODO server should retrun
        |> optional "width" D.int 100
        -- TODO server should retrun
        |> optional "height" D.int 100
        -- TODO server should retrun
        |> optional "backgroundColor" D.string "#fff"
        |> optional "name" D.string ""
        |> optional_ "personId" D.string
        |> optional "fontSize" D.float defaultFontSize
        |> optional "color" D.string def.color
        |> optional "bold" D.bool def.bold
        |> optional "url" D.string ""
        |> optional "shape" D.string "rectangle"
        |> optional_ "updateAt" D.float
