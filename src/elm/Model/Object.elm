module Model.Object exposing (..)

import CoreType exposing (..)


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


initDesk : ObjectId -> FloorId -> Position -> Size -> String -> String -> Float -> Maybe PersonId -> Float -> Object
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
        , updateAt = updateAt
        }


initLabel : ObjectId -> FloorId -> Position -> Size -> String -> String -> Float -> LabelFields -> Float -> Object
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

        -- , updateAt = 32503647600
        , updateAt = updateAt
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


urlOf : Object -> String
urlOf (Object object) =
    case object.extension of
        Desk _ ->
            ""

        Label { url } ->
            url


shapeOf : Object -> Shape
shapeOf (Object object) =
    case object.extension of
        Desk _ ->
            Rectangle

        Label { shape } ->
            shape


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
