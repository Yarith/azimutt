module PagesComponents.App.Updates.Project exposing (beginCopyToLocalStorage, createProject, deleteProject, endCopyToLocalStorage, loadProject, moveProjectToRepository, moveProjectToServer, updateProject, updateProjectUrl, useProject, useProjects)

import Conf exposing (conf)
import DataSources.SqlParser.FileParser exposing (parseSchema)
import DataSources.SqlParser.ProjectAdapter exposing (buildSourceFromSql)
import Dict
import Effect exposing (Effect)
import Libs.DomInfo exposing (DomInfo)
import Libs.List as L
import Libs.Maybe as M
import Libs.Models exposing (FileContent, TrackEvent)
import Libs.Models.HtmlId exposing (HtmlId)
import Libs.String as S
import Libs.Task as T
import List.Extra as List
import Maybe.Extra as Maybe
import Models.Project as Project exposing (Project)
import Models.Project.ProjectId exposing (ProjectId)
import Models.Project.ProjectName exposing (ProjectName)
import Models.Project.SourceKind as SourceKind
import Models.Project.Storage as Storage
import Models.Project.TableId as TableId
import Models.SourceInfo exposing (SourceInfo)
import PagesComponents.App.Models exposing (Errors, Model, Msg(..), initSwitch)
import Ports exposing (activateTooltipsAndPopovers, click, dropProject, hideModal, hideOffcanvas, observeTablesSize, saveProject, showModal, toastError, toastInfo, track, trackError)
import Set
import Shared
import Time
import Tracking exposing (events)


updateProjectUrl : Project -> Effect Msg
updateProjectUrl p =
    Just { projectId = p.id, layoutName = p.usedLayout }
        |> Shared.replaceUrlSelection
        |> Effect.fromShared


createProject : ProjectId -> SourceInfo -> FileContent -> Model -> ( Model, Effect Msg )
createProject projectId sourceInfo content model =
    let
        takenNames : List ProjectName
        takenNames =
            model.storedProjects |> List.map .name

        path : String
        path =
            sourceInfo.kind |> SourceKind.path
    in
    (if path |> String.endsWith ".sql" then
        parseSchema content
            |> Tuple.mapSecond (\( lines, schema ) -> buildSourceFromSql sourceInfo lines schema)
            |> Tuple.mapSecond (\source -> Just (Project.create projectId (S.unique takenNames source.name) source))

     else
        ( [ "Invalid file (" ++ path ++ "), expected a .sql one" ], Nothing )
    )
        |> loadProject events.createProject model


updateProject : SourceInfo -> FileContent -> Project -> ( Project, Effect Msg )
updateProject sourceInfo content project =
    let
        path : String
        path =
            sourceInfo.kind |> SourceKind.path
    in
    if path |> String.endsWith ".sql" then
        (parseSchema content
            |> Tuple.mapSecond (\( lines, schema ) -> buildSourceFromSql sourceInfo lines schema)
            |> Tuple.mapSecond
                (\newSource ->
                    project.sources
                        |> L.find (\s -> s.id == newSource.id)
                        |> Maybe.map
                            (\oldSource ->
                                ( project |> Project.updateSource newSource.id (\_ -> newSource)
                                , events.refreshSource newSource
                                , "Source <b>" ++ oldSource.name ++ "</b> updated with <b>" ++ newSource.name ++ "</b>."
                                )
                            )
                        |> Maybe.withDefault
                            ( project |> Project.addSource newSource
                            , events.addSource newSource
                            , "Source <b>" ++ newSource.name ++ "</b> added to project."
                            )
                )
        )
            |> (\( errors, ( updatedProject, event, message ) ) ->
                    ( updatedProject
                    , Effect.batch
                        [ Effect.fromCmd <|
                            Cmd.batch
                                ((errors |> List.map toastError)
                                    ++ (errors |> List.map (trackError "parse-schema"))
                                    ++ [ toastInfo message
                                       , hideOffcanvas conf.ids.settings
                                       , saveProject updatedProject
                                       , track event
                                       ]
                                )
                        , updateProjectUrl updatedProject
                        ]
                    )
               )

    else
        ( project, Effect.fromCmd <| toastError ("Invalid file (" ++ path ++ "), expected .sql") )


useProject : Project -> Model -> ( Model, Effect Msg )
useProject project model =
    ( [], Just project ) |> loadProject events.loadProject model


useProjects : List Project -> Model -> ( Model, Effect Msg )
useProjects projects model =
    case model.preselect of
        Nothing ->
            ( { model | storedProjects = projects }
            , if Maybe.isNothing model.project then
                Effect.fromCmd <| showModal conf.ids.projectSwitchModal

              else
                Effect.none
            )

        Just preselect ->
            let
                project =
                    projects
                        |> List.find (\x -> x.id == preselect.projectId)
                        |> Maybe.map
                            (\proj ->
                                preselect.layoutName
                                    |> Maybe.andThen
                                        (\layoutName ->
                                            Dict.get layoutName proj.layouts
                                                |> Maybe.map (\layout -> { proj | layout = layout, usedLayout = Just layoutName })
                                        )
                                    |> Maybe.withDefault proj
                            )
            in
            case project of
                Just proj ->
                    useProject proj
                        { model
                            | storedProjects = projects
                            , preselect = Nothing
                        }

                Nothing ->
                    ( { model
                        | storedProjects = projects
                        , preselect = Nothing
                        , project = Nothing
                      }
                    , if Maybe.isNothing model.project then
                        Effect.fromCmd <| showModal conf.ids.projectSwitchModal

                      else
                        Effect.none
                    )


deleteProject : Project -> Model -> ( Model, Effect Msg )
deleteProject project model =
    ( { model
        | storedProjects = model.storedProjects |> List.filter (\p -> not (p.id == project.id))
        , project = model.project |> Maybe.filter (\x -> x.id /= project.id)
      }
    , Effect.batch
        [ Effect.fromCmd <|
            Cmd.batch
                [ dropProject project
                , track (events.deleteProject project)
                ]
        , Effect.fromShared <| Shared.replaceUrlSelection Nothing
        ]
    )


moveProjectToServer : Project -> Model -> ( Model, Effect Msg )
moveProjectToServer project model =
    if project.storage /= Storage.LocalStorage then
        ( model, Effect.none )

    else
        let
            updatedProject =
                { project | storage = Storage.Server }
        in
        ( { model | project = Just updatedProject }
        , Effect.fromCmd <| saveProject updatedProject
        )


moveProjectToRepository : Project -> Model -> ( Model, Effect Msg )
moveProjectToRepository project model =
    if project.storage == Storage.Repository then
        ( model, Effect.none )

    else
        let
            updatedProject =
                { project | storage = Storage.Repository }
        in
        ( { model | project = Just updatedProject }
        , Effect.fromCmd <| saveProject updatedProject
        )


beginCopyToLocalStorage : Project -> Model -> ( Model, Effect Msg )
beginCopyToLocalStorage project model =
    ( { model | project = Just project }
    , Effect.fromCmd <| Cmd.batch [ saveProject project, Ports.getCloneProjectId project.id ]
    )


endCopyToLocalStorage : Time.Posix -> Project -> ProjectId -> Model -> ( Model, Effect Msg )
endCopyToLocalStorage now project newId model =
    let
        updatedProject =
            { project | storage = Storage.LocalStorage, id = newId, createdAt = now }
    in
    loadProject events.loadProject model ( [], Just updatedProject )


upsertProjectToStoredProjects : Maybe Project -> List Project -> List Project
upsertProjectToStoredProjects mproj storedProjects =
    case mproj of
        Nothing ->
            storedProjects

        Just project ->
            case List.find (\{ id } -> id == project.id) storedProjects of
                Nothing ->
                    storedProjects ++ [ project ]

                Just foundProject ->
                    if foundProject == project then
                        storedProjects

                    else
                        List.map
                            (\sp ->
                                if sp.id /= project.id then
                                    sp

                                else
                                    project
                            )
                            storedProjects


putCurrentOpenProjectToStoredProjects : Model -> List Project
putCurrentOpenProjectToStoredProjects model =
    upsertProjectToStoredProjects model.project model.storedProjects


putNewProjectToStoredProjects : Maybe Project -> List Project -> List Project
putNewProjectToStoredProjects =
    upsertProjectToStoredProjects


loadProject : (Project -> TrackEvent) -> Model -> ( Errors, Maybe Project ) -> ( Model, Effect Msg )
loadProject projectEvent model ( errors, project ) =
    ( { model
        | switch = initSwitch
        , storedProjects =
            putCurrentOpenProjectToStoredProjects model
                |> putNewProjectToStoredProjects project
        , project = project
        , domInfos =
            let
                keepTablesVisible =
                    case model.project of
                        Just mp ->
                            mp.layout.tables
                                |> List.map (.id >> TableId.toHtmlId)
                                |> Set.fromList

                        Nothing ->
                            Set.fromList []

                domInfoFilter : HtmlId -> DomInfo -> Bool
                domInfoFilter id _ =
                    Set.member id keepTablesVisible
                        || not (id |> String.startsWith "table-")
            in
            model.domInfos
                |> Dict.filter domInfoFilter
      }
    , Effect.batch
        ((errors |> List.map (toastError >> Effect.fromCmd))
            ++ (errors |> List.map (trackError "parse-project" >> Effect.fromCmd))
            ++ (project
                    |> M.mapOrElse
                        (\p ->
                            List.concat
                                [ List.map Effect.fromCmd
                                    [ if not (p.layout.tables |> List.isEmpty) then
                                        observeTablesSize (p.layout.tables |> List.map .id)

                                      else if Dict.size p.tables < 10 then
                                        T.send ShowAllTables

                                      else
                                        click conf.ids.searchInput
                                    , toastInfo ("<b>" ++ p.name ++ "</b> loaded.<br>Use the search bar to explore it")
                                    , hideModal conf.ids.projectSwitchModal
                                    , case model.project of
                                        Just prevproj ->
                                            saveProject prevproj

                                        Nothing ->
                                            Cmd.none
                                    , saveProject p
                                    , activateTooltipsAndPopovers
                                    , track (projectEvent p)
                                    ]
                                , [ updateProjectUrl p ]
                                ]
                        )
                        []
               )
        )
    )
