module API.Serialization exposing (..)

import Date
import Dict exposing (Dict)
import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)
import Util.DecodeUtil exposing (..)
import Model.Floor as Floor exposing (Floor, FloorBase)
import Model.FloorInfo as FloorInfo exposing (FloorInfo)
import Model.User as User exposing (User)
import Model.Person exposing (Person)
import Model.Object as Object exposing (Object, Shape(..))
import Model.Prototype exposing (Prototype)
import Model.SearchResult as SearchResult exposing (SearchResult)
import Model.ColorPalette as ColorPalette exposing (ColorPalette)
import Model.ObjectsChange as ObjectsChange exposing (..)
import CoreType exposing (..)
import API.Defaults as Defaults


decodeAuthToken : Decoder String
decodeAuthToken =
    D.field "accessToken" D.string


decodeColors : Decoder ColorPalette
decodeColors =
    D.map makeColorPalette (D.list decodeColorEntity)


decodePrototypes : Decoder (List Prototype)
decodePrototypes =
    D.list decodePrototype


decodeFloors : Decoder (List Floor)
decodeFloors =
    D.list decodeFloor


decodeFloorInfoList : Decoder (List FloorInfo)
decodeFloorInfoList =
    D.list decodeFloorInfo


decodePeople : Decoder (List Person)
decodePeople =
    D.list decodePerson


encodeObject : Object -> Value
encodeObject object =
    let
        { x, y } =
            Object.positionOf object

        { width, height } =
            Object.sizeOf object
    in
        E.object
            [ ( "id", E.string (Object.idOf object) )
            , ( "floorId", E.string (Object.floorIdOf object) )
            , ( "updateAt", Object.updateAtOf object |> Maybe.map E.float |> Maybe.withDefault E.null )
            , ( "type"
              , if Object.isDesk object then
                    E.string "desk"
                else
                    E.null
              )
            , ( "x", E.int x )
            , ( "y", E.int y )
            , ( "width", E.int width )
            , ( "height", E.int height )
            , ( "backgroundColor", E.string (Object.backgroundColorOf object) )
            , ( "color"
              , if Object.colorOf object == Defaults.color then
                    E.null
                else
                    E.string (Object.colorOf object)
              )
            , ( "bold"
              , if Object.isBold object then
                    E.bool True
                else
                    E.null
              )
            , ( "url", E.string (Object.urlOf object) )
            , ( "shape", encodeShape (Object.shapeOf object) )
            , ( "name", E.string (Object.nameOf object) )
            , ( "fontSize"
              , if Object.fontSizeOf object == Defaults.fontSize then
                    E.null
                else
                    E.float (Object.fontSizeOf object)
              )
            , ( "personId"
              , case Object.relatedPerson object of
                    Just personId ->
                        E.string personId

                    Nothing ->
                        E.null
              )
            ]


encodeShape : Shape -> Value
encodeShape shape =
    case shape of
        Object.Rectangle ->
            E.null

        Object.Ellipse ->
            E.string "Ellipse"


encodeObjectModification : ObjectModification -> Value
encodeObjectModification mod =
    E.object
        [ ( "old", encodeObject mod.old )
        , ( "new", encodeObject mod.new )
        ]


encodeFloor : Floor -> Value
encodeFloor floor =
    E.object
        [ ( "id", E.string floor.id )
        , ( "name", E.string floor.name )
        , ( "ord", E.int floor.ord )
        , ( "width", E.int floor.width )
        , ( "height", E.int floor.height )
        , ( "realWidth", Maybe.withDefault E.null <| Maybe.map (E.int << Tuple.first) floor.realSize )
        , ( "realHeight", Maybe.withDefault E.null <| Maybe.map (E.int << Tuple.second) floor.realSize )
        , ( "temporary", E.bool floor.temporary )
        , ( "image", Maybe.withDefault E.null <| Maybe.map E.string floor.image )
        , ( "flipImage", E.bool floor.flipImage )
        ]


encodeObjectsChange : ObjectsChange -> Value
encodeObjectsChange change =
    change
        |> ObjectsChange.toList
        |> List.map encodeObjectChange
        |> E.list


encodeObjectChange : ObjectChange Object -> Value
encodeObjectChange change =
    case change of
        ObjectsChange.Added object ->
            E.object
                [ ( "flag", E.string "added" )
                , ( "object", encodeObject object )
                ]

        ObjectsChange.Modified object ->
            E.object
                [ ( "flag", E.string "modified" )
                , ( "object", encodeObject object )
                ]

        ObjectsChange.Deleted object ->
            E.object
                [ ( "flag", E.string "deleted" )
                , ( "object", encodeObject object )
                ]


decodeObjectsChange : Decoder ObjectsChange
decodeObjectsChange =
    D.list decodeObjectChange
        |> D.map ObjectsChange.fromList


decodeObjectChange : Decoder ( ObjectId, ObjectChange Object )
decodeObjectChange =
    D.map2
        (\flag object ->
            if flag == "added" then
                ( Object.idOf object, ObjectsChange.Added object )
            else if flag == "modified" then
                ( Object.idOf object, ObjectsChange.Modified object )
            else
                ( Object.idOf object, ObjectsChange.Deleted object )
        )
        (D.field "flag" D.string)
        (D.field "object" decodeObject)


encodeLogin : String -> String -> Value
encodeLogin userId pass =
    E.object
        [ ( "userId", E.string userId )
        , ( "password", E.string pass )
        ]


decodeUser : Decoder User
decodeUser =
    D.oneOf
        [ D.map2
            (\role person ->
                if role == "admin" then
                    User.admin person
                else
                    User.general person
            )
            (D.field "role" D.string)
            (D.field "person" decodePerson)
        , D.succeed User.guest
        ]


decodeUsers : Decoder (List User)
decodeUsers =
    D.list decodeUser


decodeColorEntity : Decoder ColorEntity
decodeColorEntity =
    decode ColorEntity
        |> required "ord" D.int
        |> required "type" D.string
        |> required "color" D.string


decodePerson : Decoder Person
decodePerson =
    decode Person
        |> required "id" D.string
        |> required "name" D.string
        |> required "post" D.string
        |> optional_ "mail" D.string
        |> optional_ "tel1" D.string
        |> optional_ "tel2" D.string
        |> optional_ "image" D.string


{-| this is needed for historical reason...
-}
decodePersonFromProfileService : Decoder Person
decodePersonFromProfileService =
    decode Person
        |> required "userId" D.string
        |> required "name" D.string
        |> required "post" D.string
        |> optional_ "mail" D.string
        |> optional_ "extensionPhone" D.string
        |> optional_ "cellPhone" D.string
        |> optional_ "picture" D.string


decodeObject : Decoder Object
decodeObject =
    decode
        (\id floorId updateAt tipe x y width height backgroundColor name personId fontSize color bold url shape ->
            if tipe == "desk" then
                Object.initDesk id floorId (Position x y) (Size width height) backgroundColor name fontSize (Just updateAt) personId
            else
                Object.initLabel id
                    floorId
                    (Position x y)
                    (Size width height)
                    backgroundColor
                    name
                    fontSize
                    (Just updateAt)
                    (Object.LabelFields color
                        bold
                        url
                        (if shape == "Rectangle" then
                            Object.Rectangle
                         else
                            Object.Ellipse
                        )
                    )
        )
        |> required "id" D.string
        |> required "floorId" D.string
        |> required "updateAt" D.float
        |> optional "type" D.string "desk"
        |> required "x" D.int
        |> required "y" D.int
        |> required "width" D.int
        |> required "height" D.int
        |> required "backgroundColor" D.string
        |> optional "name" D.string ""
        |> optional_ "personId" D.string
        |> optional "fontSize" D.float Object.defaultFontSize
        |> optional "color" D.string Defaults.color
        |> optional "bold" D.bool Defaults.bold
        |> optional "url" D.string ""
        |> optional "shape" D.string "Rectangle"


decodeSearchResult : Decoder (Maybe SearchResult)
decodeSearchResult =
    decode
        (\maybePersonId maybeObjectAndFloorId ->
            case ( maybePersonId, maybeObjectAndFloorId ) of
                ( _, Just ( object, floorId ) ) ->
                    Just <| SearchResult.Object object floorId

                ( Just personId, Nothing ) ->
                    Just <| SearchResult.MissingPerson personId

                _ ->
                    Nothing
        )
        |> optional_ "personId" D.string
        |> optional_ "objectAndFloorId" (tuple2 (,) decodeObject D.string)


decodeSearchResults : Decoder ( List SearchResult, List Person )
decodeSearchResults =
    D.map2 (,)
        (D.field "result" (D.map (List.filterMap identity) (D.list decodeSearchResult)))
        (D.field "people" decodePeople)


decodeFloor : Decoder Floor
decodeFloor =
    decode
        (\id name ord objects width height realWidth realHeight image flipImage temporary updateBy updateAt ->
            { id = id
            , name = name
            , ord = ord
            , objects = Dict.empty
            , width = width
            , height = height
            , image = image
            , flipImage = flipImage
            , realSize = Maybe.map2 (,) realWidth realHeight
            , temporary = temporary
            , update = Maybe.map2 (\by at -> { by = by, at = Date.fromTime at }) updateBy updateAt
            }
                |> Floor.addObjects objects
        )
        |> required "id" D.string
        |> required "name" D.string
        |> required "ord" D.int
        |> required "objects" (D.list decodeObject)
        |> required "width" D.int
        |> required "height" D.int
        |> optional_ "realWidth" D.int
        |> optional_ "realHeight" D.int
        |> optional_ "image" D.string
        |> required "flipImage" D.bool
        |> required "temporary" D.bool
        |> optional_ "updateBy" D.string
        |> optional_ "updateAt" D.float


decodeFloorBase : Decoder FloorBase
decodeFloorBase =
    decode FloorBase
        |> required "id" D.string
        |> required "temporary" D.bool
        |> required "name" D.string
        |> required "ord" D.int


decodeFloorInfo : Decoder FloorInfo
decodeFloorInfo =
    tuple2 FloorInfo.init (D.maybe decodeFloorBase) decodeFloorBase


decodePrototype : Decoder Prototype
decodePrototype =
    decode
        (\id backgroundColor color name width height fontSize shape ->
            { id = id
            , name = name
            , backgroundColor = backgroundColor
            , color = color
            , width = width
            , height = height
            , fontSize = fontSize
            , shape =
                if shape == "Ellipse" then
                    Ellipse
                else
                    Rectangle
            , personId = Nothing
            }
        )
        |> required "id" D.string
        |> required "backgroundColor" D.string
        |> optional "color" D.string Defaults.color
        |> optional "name" D.string ""
        |> required "width" D.int
        |> required "height" D.int
        |> optional "fontSize" D.float Defaults.fontSize
        |> optional "shape" D.string "Rectangle"


encodePrototype : Prototype -> Value
encodePrototype { id, color, backgroundColor, name, width, height, fontSize, shape } =
    E.object
        [ ( "id", E.string id )
        , ( "color"
          , if color == Defaults.color then
                E.null
            else
                E.string color
          )
        , ( "backgroundColor", E.string backgroundColor )
        , ( "name", E.string name )
        , ( "width", E.int width )
        , ( "height", E.int height )
        , ( "fontSize"
          , if fontSize == Defaults.fontSize then
                E.null
            else
                E.float fontSize
          )
        , ( "shape", encodeShape shape )
        ]


encodeColorPalette : ColorPalette -> Value
encodeColorPalette colorPalette =
    encodeColorEntities (makeColorEntities colorPalette)


encodeColorEntities : List ColorEntity -> Value
encodeColorEntities entities =
    E.list (List.map encodeColorEntitity entities)


encodeColorEntitity : ColorEntity -> E.Value
encodeColorEntitity entity =
    E.object
        [ ( "ord", E.int entity.ord )
        , ( "color", E.string entity.color )
        , ( "type", E.string entity.type_ )
        ]


encodePrototypes : List Prototype -> E.Value
encodePrototypes prototypes =
    E.list (List.map encodePrototype prototypes)


type alias ColorEntity =
    { ord : Int
    , type_ : String
    , color : String
    }


makeColorPalette : List ColorEntity -> ColorPalette
makeColorPalette entities =
    let
        sorted =
            List.sortBy (.ord) entities

        backgroundColors =
            List.filterMap
                (\e ->
                    if e.type_ == "backgroundColor" then
                        Just e.color
                    else
                        Nothing
                )
                sorted

        textColors =
            List.filterMap
                (\e ->
                    if e.type_ == "color" then
                        Just e.color
                    else
                        Nothing
                )
                sorted
    in
        { backgroundColors = backgroundColors
        , textColors = textColors
        }


makeColorEntities : ColorPalette -> List ColorEntity
makeColorEntities colorPalette =
    List.indexedMap (makeColorEntity "color") colorPalette.textColors
        ++ List.indexedMap (makeColorEntity "backgroundColor") colorPalette.backgroundColors


makeColorEntity : String -> Int -> String -> ColorEntity
makeColorEntity type_ ord color =
    { ord = ord, color = color, type_ = type_ }
