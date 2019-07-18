module API.Defaults exposing (bold, color, fontSize, shape, tipe)

import Model.Object exposing (Shape)


fontSize : Float
fontSize =
    20.0


tipe : String
tipe =
    "desk"


shape : Shape
shape =
    Model.Object.Rectangle


color : String
color =
    "#000"


bold : Bool
bold =
    False
