module Pages.App exposing (Model, Msg, page)

import Browser.Events
import Conf exposing (conf)
import Dict
import Effect exposing (Effect)
import Gen.Params.App exposing (Params)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Libs.Bool as B
import Libs.List as L
import Libs.Maybe as M
import Libs.Models.Position as Position
import List.Extra
import Maybe.Extra as Maybe
import Models.Project.Storage as Storage
import Page
import PagesComponents.App.Commands.GetTime exposing (getTime)
import PagesComponents.App.Commands.GetZone exposing (getZone)
import PagesComponents.App.Models as Models exposing (CursorMode(..), DragState, Model, Msg(..), SourceMsg(..), VirtualRelation, VirtualRelationMsg(..), initConfirm, initHover, initSwitch, initTimeInfo)
import PagesComponents.App.Updates exposing (updateSizes)
import PagesComponents.App.Updates.Canvas exposing (fitCanvas, handleWheel, resetCanvas, zoomCanvas)
import PagesComponents.App.Updates.Drag exposing (dragEnd, dragMove, dragStart)
import PagesComponents.App.Updates.FindPath exposing (handleFindPath)
import PagesComponents.App.Updates.Helpers exposing (setCanvas, setCurrentLayout, setProject, setProjectWithCmd, setTableInList, setTables, setTime)
import PagesComponents.App.Updates.Layout exposing (handleLayout)
import PagesComponents.App.Updates.PortMsg exposing (handlePortMsg)
import PagesComponents.App.Updates.Project as Project exposing (deleteProject, useProject, useProjects)
import PagesComponents.App.Updates.Settings exposing (handleSettings)
import PagesComponents.App.Updates.Source exposing (handleSource)
import PagesComponents.App.Updates.Table exposing (hideAllTables, hideColumn, hideColumns, hideTable, hoverNextColumn, showAllTables, showColumn, showColumns, showTable, showTables, sortColumns)
import PagesComponents.App.Updates.VirtualRelation exposing (handleVirtualRelation)
import PagesComponents.App.View exposing (viewApp)
import PagesComponents.Helpers as Helpers
import Ports exposing (JsMsg(..), activateTooltipsAndPopovers, click, hideModal, hideOffcanvas, listenHotkeys, loadProjects, observeSize, onJsMessage, showModal, trackPage)
import Request
import Shared
import Time
import Tracking
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared request =
    Page.advanced
        { init = init request
        , update = update shared
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Models.Model


type alias Msg =
    Models.Msg



-- INIT


init : Request.With Params -> ( Model, Effect Msg )
init { query } =
    ( { time = initTimeInfo
      , preselect = Models.parsePreselect query
      , switch = initSwitch
      , storedProjects = []
      , project = Nothing
      , search = ""
      , newLayout = Nothing
      , findPath = Nothing
      , virtualRelation = Nothing
      , confirm = initConfirm
      , domInfos = Dict.empty
      , cursorMode = Select
      , selection = Nothing
      , dragState = Nothing
      , hover = initHover
      }
    , Effect.fromCmd <|
        Cmd.batch
            [ observeSize conf.ids.erd
            , loadProjects
            , getZone
            , getTime
            , listenHotkeys conf.hotkeys
            , trackPage "app"
            ]
    )



-- UPDATE


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        -- each case should be one line or call a function in Update file
        TimeChanged time ->
            ( model |> setTime (\t -> { t | now = time }), Effect.none )

        ZoneChanged zone ->
            ( model |> setTime (\t -> { t | zone = zone }), Effect.none )

        SizesChanged sizes ->
            model |> updateSizes sizes

        SourceMsg m ->
            model |> handleSource m

        MoveProjectToServer project ->
            model |> Project.moveProjectToServer project

        MoveProjectToRepository project ->
            model |> Project.moveProjectToRepository project

        BeginCopyToLocalStorage project ->
            model |> Project.beginCopyToLocalStorage project

        EndCopyToLocalStorage now srcId newId ->
            case Maybe.filter (.id >> (==) srcId) model.project of
                Just project ->
                    model |> Project.endCopyToLocalStorage now project newId

                Nothing ->
                    ( model, Effect.none )

        ChangeProject ->
            ( model
            , Effect.fromCmd <|
                Cmd.batch
                    [ hideOffcanvas conf.ids.menu
                    , showModal conf.ids.projectSwitchModal
                    , loadProjects
                    ]
            )

        ProjectsLoaded projects ->
            model |> useProjects projects

        UseProject project ->
            model |> useProject project

        DeleteProject project ->
            if shared.isDev || project.storage /= Storage.Repository then
                model |> deleteProject project

            else
                ( model, Effect.fromCmd <| Ports.toastWarning "Repository projects can only be removed on development machines" )

        ChangedSearch search ->
            ( { model | search = search }, Effect.none )

        SelectTable id ctrl ->
            let
                setTableSelected t value =
                    if t.selected /= value then
                        { t | selected = value }

                    else
                        t

                updateTable t =
                    if t.id /= id then
                        setTableSelected t (ctrl && t.selected)

                    else if ctrl then
                        setTableSelected t (not t.selected)

                    else if not t.selected then
                        setTableSelected t True

                    else
                        t

                updateTables tables =
                    if not ctrl && List.any (\t -> t.id == id && t.selected) tables then
                        -- Clicking on a selected table without control should keep the current state
                        tables

                    else
                        List.map updateTable tables
            in
            ( model |> setCurrentLayout (setTables updateTables), Effect.none )

        SelectAllTables ->
            ( model |> setCurrentLayout (setTables (List.map (\t -> { t | selected = True }))), Effect.none )

        HideTable id ->
            ( model |> setCurrentLayout (hideTable id), Effect.none )

        ShowTable id ->
            model |> setProjectWithCmd (showTable id)

        TableOrder id index ->
            ( model |> setCurrentLayout (setTables (\tables -> tables |> L.moveBy .id id (List.length tables - 1 - index))), Effect.none )

        ShowTables ids ->
            model |> setProjectWithCmd (showTables ids)

        --HideTables ids ->
        --    ( model |> setCurrentLayout (hideTables ids), Cmd.none )
        InitializedTable id position ->
            ( model |> setCurrentLayout (setTableInList .id id (\t -> { t | position = position })), Effect.none )

        HideAllTables ->
            ( model |> setCurrentLayout hideAllTables, Effect.none )

        ShowAllTables ->
            model |> setProjectWithCmd showAllTables

        HideColumn { table, column } ->
            ( model |> hoverNextColumn table column |> setCurrentLayout (hideColumn table column), Effect.none )

        ShowColumn { table, column } ->
            ( model |> setCurrentLayout (showColumn table column), Effect.fromCmd <| activateTooltipsAndPopovers )

        SortColumns id kind ->
            ( model |> setProject (sortColumns id kind), Effect.fromCmd <| activateTooltipsAndPopovers )

        HideColumns id kind ->
            ( model |> setProject (hideColumns id kind), Effect.none )

        ShowColumns id kind ->
            ( model |> setProject (showColumns id kind), Effect.fromCmd <| activateTooltipsAndPopovers )

        HoverTable t ->
            ( { model | hover = model.hover |> (\h -> { h | table = t }) }, Effect.none )

        HoverColumn c ->
            ( { model | hover = model.hover |> (\h -> { h | column = c }) }, Effect.none )

        OnWheel event ->
            ( model |> setCurrentLayout (setCanvas (handleWheel event)), Effect.none )

        Zoom delta ->
            ( model |> setCurrentLayout (setCanvas (zoomCanvas model.domInfos delta)), Effect.none )

        FitContent ->
            ( model |> setCurrentLayout (fitCanvas model.domInfos), Effect.none )

        ResetCanvas ->
            model |> setProjectWithCmd resetCanvas

        DragStart id ctrl pos ->
            model |> dragStart id ctrl pos

        DragMove ctrl pos ->
            model |> dragMove ctrl pos

        DragEnd ctrl pos ->
            model |> dragEnd ctrl pos

        CursorMode mode ->
            ( { model | cursorMode = mode }, Effect.none )

        LayoutMsg m ->
            model |> handleLayout m

        FindPathMsg m ->
            model |> handleFindPath m

        VirtualRelationMsg m ->
            model |> handleVirtualRelation m

        SettingsMsg m ->
            model |> handleSettings m

        OpenConfirm confirm ->
            ( { model | confirm = confirm }, Effect.fromCmd <| showModal conf.ids.confirm )

        OnConfirm answer cmd ->
            ( { model | confirm = initConfirm }, Effect.fromCmd <| B.cond answer cmd Cmd.none )

        JsMessage m ->
            ( model, model |> handlePortMsg m )

        Noop ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ Time.every (10 * 1000) TimeChanged
         , onJsMessage JsMessage
         ]
            ++ dragSubscriptions model.dragState
            ++ virtualRelationSubscription model.virtualRelation
        )


dragSubscriptions : Maybe DragState -> List (Sub Msg)
dragSubscriptions drag =
    case drag of
        Nothing ->
            []

        Just _ ->
            [ Browser.Events.onMouseMove (Decode.map (\x -> x.pagePos |> Position.fromTuple |> DragMove x.keys.ctrl) Mouse.eventDecoder)
            , Browser.Events.onMouseUp (Decode.map (\x -> x.pagePos |> Position.fromTuple |> DragEnd x.keys.ctrl) Mouse.eventDecoder)
            ]


virtualRelationSubscription : Maybe VirtualRelation -> List (Sub Msg)
virtualRelationSubscription virtualRelation =
    case virtualRelation |> Maybe.map .src of
        Nothing ->
            []

        Just _ ->
            [ Browser.Events.onMouseMove (Decode.map (.pagePos >> Position.fromTuple >> VRMove >> VirtualRelationMsg) Mouse.eventDecoder) ]



-- VIEW


view : Model -> View Msg
view model =
    { title = model.project |> M.mapOrElse (\p -> p.name ++ " - Azimutt") "Azimutt - Explore your database schema"
    , body = Helpers.root (viewApp model)
    }
