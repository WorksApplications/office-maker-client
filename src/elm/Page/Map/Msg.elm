module Page.Map.Msg exposing (..)

import API.Cache exposing (UserState)
import Component.FloorDeleter as FloorDeleter
import Component.FloorProperty as FloorProperty
import Component.Header as Header exposing (..)
import Component.ImageLoader as ImageLoader
import ContextMenu
import CoreType exposing (..)
import Debounce
import Model.ColorPalette exposing (ColorPalette)
import Model.Floor exposing (Floor, FloorBase)
import Model.FloorInfo exposing (FloorInfo)
import Model.I18n exposing (Language(..))
import Model.Information exposing (Information(..))
import Model.Mode exposing (EditingMode(..), Mode(..))
import Model.Object as Object exposing (..)
import Model.Person exposing (Person)
import Model.Prototype exposing (Prototype)
import Model.Prototypes as Prototypes
import Model.SaveRequest exposing (SaveRequest(..))
import Model.SearchResult exposing (SearchResult)
import Model.User exposing (User)
import Page.Map.ClipboardOptionsView as ClipboardOptionsView
import Page.Map.ContextMenuContext exposing (ContextMenuContext)
import Page.Map.URL exposing (URL)
import Time exposing (Time)
import Util.File exposing (File)


type alias Id =
    String


{-| TODO temporary here for sefe refactoring
-}
type ObjectNameInputMsg
    = NoOperation
    | CaretPosition Int
    | InputName ObjectId String Int
    | KeydownOnNameInput (List Person) ( Int, Int )
    | KeyupOnNameInput Int
    | SelectCandidate ObjectId PersonId
    | UnsetPerson ObjectId


type Msg
    = NoOp
    | UrlUpdate (Result String URL)
    | UserLoaded Bool User
    | Initialize Bool (Maybe String) UserState
    | FloorsInfoLoaded Bool (List FloorInfo)
    | FloorLoaded (Maybe Floor)
    | ColorsLoaded ColorPalette
    | PrototypesLoaded (List Prototype)
    | ImageSaved String Int Int
    | RequestSave SaveRequest
    | SaveFloorDebounceMsg Debounce.Msg
    | ObjectsSaved
    | UnlockSaveFloor
    | FloorSaved FloorBase
    | FloorPublished Floor
    | ClickOnCanvas
    | MouseDownOnCanvas Position
    | MouseUpOnCanvas Position
    | FocusCanvas
    | MouseDownOnObject Bool Bool Bool Id Position
    | MouseUpOnObject Id Position
    | MouseDownOnResizeGrip Id Position
    | StartEditObject Id
    | Ctrl Bool
    | SelectBackgroundColor String
    | SelectColor String
    | SelectShape Object.Shape
    | SelectFontSize Float
    | InputObjectUrl (List ObjectId) String
    | ObjectNameInputMsg ObjectNameInputMsg
    | BeforeContextMenuOnObject Id Msg
    | ContextMenuMsg (ContextMenu.Msg ContextMenuContext)
    | GoToFloor String Bool
    | SelectSamePost String
    | SearchByPost String
    | GotSamePostPeople (List Person)
    | SelectIsland Id
    | SelectSameColor Id
    | WindowSize Size
    | MouseWheel Float Position
    | ChangeMode EditingMode
    | PrototypesMsg Prototypes.Msg
    | ClipboardOptionsMsg ( ClipboardOptionsView.Form, Maybe Size )
    | RegisterPrototype Id
    | FloorPropertyMsg FloorProperty.Msg
    | RotateObjects (List ObjectId)
    | FirstNameOnly (List Id)
    | RemoveSpaces (List Id)
    | HeaderMsg Header.Msg
    | SignIn
    | SignOut
    | ToggleEditing
    | TogglePrintView
    | SelectLang Language
    | UpdateSearchQuery String
    | SubmitSearch
    | StartSearch
    | GotSearchResult (List SearchResult) (List Person)
    | SelectSearchResult ObjectId FloorId (Maybe PersonId)
    | CloseSearchResult
    | StartDraggingFromMissingPerson Prototype String String
    | StartDraggingFromExistingObject Prototype Id String (Maybe String) String
    | CachePeople (List Person)
    | RequestCandidate Id String
    | SearchCandidateDebounceMsg Debounce.Msg
    | GotCandidateSelection Id (List Person)
    | GotMatchingList (List ( Id, List Person ))
    | UpdatePersonCandidate Id (List Id)
    | PreparePublish
    | GotDiffSource ( Floor, Maybe Floor )
    | CloseDiff
    | ConfirmDiff
    | ClosePopup
    | ShowDetailForObject Id
    | CreateNewFloor
    | CopyFloor FloorId Bool
    | EmulateClick Id Bool Time
    | TokenRemoved
    | Undo
    | Redo
    | Focused
    | PasteFromClipboard String
    | SyncFloor
    | MouseMove Position
    | MouseUp
    | ImageLoaderMsg ImageLoader.Msg
    | GotFileWithDataURL File String
    | FloorDeleterMsg FloorDeleter.Msg
    | FloorDeleted Floor
    | InsertEmoji String
    | ChangeToObjectUrl ObjectId
    | SetTransition Bool
    | Copy
    | Cut
    | Delete
    | MoveSelecedObjectsToward Direction
    | ShiftSelectionByTab
    | ExpandOrShrinkToward Direction
    | Print
    | FlipFloor
    | ShowInformation Information
    | GotNewToken (Maybe String)
