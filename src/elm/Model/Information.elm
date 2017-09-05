module Model.Information exposing (..)

import Dom
import Util.File as File
import API.API as API exposing (..)


type Information
    = APIError API.Error
    | FileError File.Error
    | HtmlError Dom.Error
    | PasteError String
    | Success String
    | InProgress String
    | NoInformation
