module Model.Information exposing (..)

import API.API as API exposing (..)
import Dom
import Util.File as File


type Information
    = APIError API.Error
    | FileError File.Error
    | HtmlError Dom.Error
    | PasteError String
    | Success String
    | PublishInProgress String
    | PublishedFloor String
    | NoInformation
