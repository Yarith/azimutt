module PagesComponents.App.Models exposing (Confirm, CursorMode(..), DragId, DragState, Error, Errors, FindPathMsg(..), Hover, LayoutMsg(..), Model, Msg(..), Preselect, Search, SettingsMsg(..), SourceMsg(..), Switch, TimeInfo, VirtualRelation, VirtualRelationMsg(..), initConfirm, initHover, initSwitch, initTimeInfo, parsePreselect)

import Dict exposing (Dict)
import FileValue exposing (File)
import Html exposing (Html, text)
import Libs.Area exposing (Area)
import Libs.Delta exposing (Delta)
import Libs.DomInfo exposing (DomInfo)
import Libs.Html.Events exposing (WheelEvent)
import Libs.Models exposing (FileContent, SizeChange, ZoomDelta)
import Libs.Models.FileUrl exposing (FileUrl)
import Libs.Models.HtmlId exposing (HtmlId)
import Libs.Models.Position exposing (Position)
import Libs.Task as T
import Models.ColumnOrder exposing (ColumnOrder)
import Models.Project exposing (Project)
import Models.Project.ColumnRef exposing (ColumnRef)
import Models.Project.FindPath exposing (FindPath)
import Models.Project.FindPathSettings exposing (FindPathSettings)
import Models.Project.LayoutName exposing (LayoutName)
import Models.Project.ProjectId exposing (ProjectId)
import Models.Project.ProjectName exposing (ProjectName)
import Models.Project.Relation exposing (Relation)
import Models.Project.SampleName exposing (SampleName)
import Models.Project.SchemaName exposing (SchemaName)
import Models.Project.Source exposing (Source)
import Models.Project.SourceId exposing (SourceId)
import Models.Project.Table exposing (Table)
import Models.Project.TableId exposing (TableId)
import Models.SourceInfo exposing (SourceInfo)
import Ports exposing (JsMsg)
import Time


type alias Model =
    { time : TimeInfo
    , preselect : Maybe Preselect
    , switch : Switch
    , storedProjects : List Project
    , project : Maybe Project
    , search : Search
    , newLayout : Maybe LayoutName
    , findPath : Maybe FindPath
    , virtualRelation : Maybe VirtualRelation
    , confirm : Confirm
    , domInfos : Dict HtmlId DomInfo
    , cursorMode : CursorMode
    , selection : Maybe Area
    , dragState : Maybe DragState
    , hover : Hover
    }


type alias Preselect =
    { projectId : ProjectId
    , layoutName : Maybe LayoutName
    }


parsePreselectHelp : Dict String String -> ProjectId -> Preselect
parsePreselectHelp query projectId =
    { projectId = projectId
    , layoutName = Dict.get "layoutName" query
    }


parsePreselect : Dict String String -> Maybe Preselect
parsePreselect query =
    Dict.get "projectId" query
        |> Maybe.map (parsePreselectHelp query)


type alias VirtualRelation =
    { src : Maybe ColumnRef, mouse : Position }


type CursorMode
    = Drag
    | Select


type alias DragState =
    { id : DragId, init : Position, last : Position, delta : Delta }


type alias Ctrl =
    Bool


type Msg
    = TimeChanged Time.Posix
    | ZoneChanged Time.Zone
    | SizesChanged (List SizeChange)
    | ChangeProject
    | ProjectsLoaded (List Project)
    | SourceMsg SourceMsg
    | MoveProjectToServer Project
    | MoveProjectToRepository Project
    | BeginCopyToLocalStorage Project
    | EndCopyToLocalStorage Time.Posix ProjectId ProjectId
    | DeleteProject Project
    | UseProject Project
    | ChangedSearch Search
    | SelectTable TableId Bool
    | SelectAllTables
    | HideTable TableId
    | ShowTable TableId
    | TableOrder TableId Int
    | ShowTables (List TableId)
      -- | HideTables (List TableId)
    | InitializedTable TableId Position
    | HideAllTables
    | ShowAllTables
    | HideColumn ColumnRef
    | ShowColumn ColumnRef
    | SortColumns TableId ColumnOrder
    | HideColumns TableId String
    | ShowColumns TableId String
    | HoverTable (Maybe TableId)
    | HoverColumn (Maybe ColumnRef)
    | OnWheel WheelEvent
    | Zoom ZoomDelta
    | FitContent
    | ResetCanvas
    | DragStart DragId Ctrl Position
    | DragMove Ctrl Position
    | DragEnd Ctrl Position
    | CursorMode CursorMode
    | FindPathMsg FindPathMsg
    | VirtualRelationMsg VirtualRelationMsg
    | LayoutMsg LayoutMsg
    | SettingsMsg SettingsMsg
    | OpenConfirm Confirm
    | OnConfirm Bool (Cmd Msg)
    | JsMessage JsMsg
    | Noop


type SettingsMsg
    = ToggleSchema SchemaName
    | ToggleRemoveViews
    | UpdateRemovedTables String
    | UpdateHiddenColumns String
    | UpdateColumnOrder ColumnOrder
    | UpdateProjectName ProjectName


type SourceMsg
    = FileDragOver File (List File)
    | FileDragLeave
    | LoadLocalFile (Maybe ProjectId) (Maybe SourceId) File
    | LoadRemoteFile (Maybe ProjectId) (Maybe SourceId) FileUrl
    | LoadSample SampleName
    | FileLoaded ProjectId SourceInfo FileContent
    | ToggleSource Source
    | CreateSource Source String
    | DeleteSource Source


type LayoutMsg
    = LNew LayoutName
    | LCreate LayoutName
    | LLoad LayoutName
    | LUnload
    | LUpdate LayoutName
    | LDelete LayoutName


type FindPathMsg
    = FPInit (Maybe TableId) (Maybe TableId)
    | FPUpdateFrom (Maybe TableId)
    | FPUpdateTo (Maybe TableId)
    | FPSearch
    | FPCompute (Dict TableId Table) (List Relation) TableId TableId FindPathSettings
    | FPSettingsUpdate FindPathSettings


type VirtualRelationMsg
    = VRCreate
    | VRUpdate ColumnRef Position
    | VRMove Position
    | VRCancel


type alias TimeInfo =
    { zone : Time.Zone, now : Time.Posix }


type alias Switch =
    { loading : Bool }


type alias Confirm =
    { content : Html Msg, cmd : Cmd Msg }


type alias Hover =
    { table : Maybe TableId, column : Maybe ColumnRef }


type alias Search =
    String


type alias DragId =
    HtmlId


type alias Error =
    String


type alias Errors =
    List Error


initTimeInfo : TimeInfo
initTimeInfo =
    { zone = Time.utc, now = Time.millisToPosix 0 }


initSwitch : Switch
initSwitch =
    { loading = False }


initConfirm : Confirm
initConfirm =
    { content = text "No text", cmd = T.send Noop }


initHover : Hover
initHover =
    { table = Nothing, column = Nothing }
