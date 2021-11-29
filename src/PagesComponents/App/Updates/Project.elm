module PagesComponents.App.Updates.Project exposing (beginCopyToLocalStorage, createProject, deleteProject, endCopyToLocalStorage, loadProject, moveProjectToRepository, moveProjectToServer, updateProject, useProject)

import Conf exposing (conf)
import DataSources.SqlParser.FileParser exposing (parseSchema)
import DataSources.SqlParser.ProjectAdapter exposing (buildSourceFromSql)
import Dict
import Libs.DomInfo exposing (DomInfo)
import Libs.List as L
import Libs.Maybe as M
import Libs.Models exposing (FileContent, TrackEvent)
import Libs.Models.HtmlId exposing (HtmlId)
import Libs.String as S
import Libs.Task as T
import List.Extra as List
import Models.Project as Project exposing (Project)
import Models.Project.ProjectId exposing (ProjectId)
import Models.Project.ProjectName exposing (ProjectName)
import Models.Project.SourceKind as SourceKind
import Models.Project.Storage as Storage
import Models.Project.TableId as TableId
import Models.SourceInfo exposing (SourceInfo)
import PagesComponents.App.Models exposing (Errors, Model, Msg(..), initSwitch)
import Ports exposing (activateTooltipsAndPopovers, click, dropProject, hideModal, hideOffcanvas, observeTablesSize, saveProject, toastError, toastInfo, track, trackError)
import Set
import Time
import Tracking exposing (events)


createProject : ProjectId -> SourceInfo -> FileContent -> Model -> ( Model, Cmd Msg )
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


updateProject : SourceInfo -> FileContent -> Project -> ( Project, Cmd Msg )
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
                    , Cmd.batch
                        ((errors |> List.map toastError)
                            ++ (errors |> List.map (trackError "parse-schema"))
                            ++ [ toastInfo message
                               , hideOffcanvas conf.ids.settings
                               , saveProject updatedProject
                               , track event
                               ]
                        )
                    )
               )

    else
        ( project, toastError ("Invalid file (" ++ path ++ "), expected .sql") )


useProject : Project -> Model -> ( Model, Cmd Msg )
useProject project model =
    ( [], Just project ) |> loadProject events.loadProject model


deleteProject : Project -> Model -> ( Model, Cmd Msg )
deleteProject project model =
    ( { model | storedProjects = model.storedProjects |> List.filter (\p -> not (p.id == project.id)) }, Cmd.batch [ dropProject project, track (events.deleteProject project) ] )


moveProjectToServer : Project -> Model -> ( Model, Cmd Msg )
moveProjectToServer project model =
    if project.storage /= Storage.LocalStorage then
        ( model, Cmd.none )

    else
        let
            updatedProject =
                { project | storage = Storage.Server }
        in
        ( { model | project = Just updatedProject }, saveProject updatedProject )


moveProjectToRepository : Project -> Model -> ( Model, Cmd Msg )
moveProjectToRepository project model =
    if project.storage == Storage.Repository then
        ( model, Cmd.none )

    else
        let
            updatedProject =
                { project | storage = Storage.Repository }
        in
        ( { model | project = Just updatedProject }, saveProject updatedProject )


beginCopyToLocalStorage : Project -> Model -> ( Model, Cmd Msg )
beginCopyToLocalStorage project model =
    ( { model | project = Just project }, Cmd.batch [ saveProject project, Ports.getCloneProjectId project.id ] )


endCopyToLocalStorage : Time.Posix -> Project -> ProjectId -> Model -> ( Model, Cmd Msg )
endCopyToLocalStorage now project newId model =
    let
        updatedProject =
            { project | storage = Storage.LocalStorage, id = newId, createdAt = now }
    in
    ( { model | project = Just updatedProject, storedProjects = updatedProject :: model.storedProjects }, saveProject updatedProject )


putCurrentOpenProjectToStoredProjects : Model -> List Project
putCurrentOpenProjectToStoredProjects model =
    case model.project of
        Nothing ->
            model.storedProjects

        Just project ->
            model.storedProjects
                |> List.map
                    (\sp ->
                        if sp.id /= project.id then
                            sp

                        else
                            project
                    )


loadProject : (Project -> TrackEvent) -> Model -> ( Errors, Maybe Project ) -> ( Model, Cmd Msg )
loadProject projectEvent model ( errors, project ) =
    ( { model
        | switch = initSwitch
        , storedProjects =
            let
                storedProjects =
                    putCurrentOpenProjectToStoredProjects model
            in
            storedProjects |> L.appendOn (project |> M.filter (\p -> storedProjects |> List.all (\s -> s.id /= p.id))) identity
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
    , Cmd.batch
        ((errors |> List.map toastError)
            ++ (errors |> List.map (trackError "parse-project"))
            ++ (project
                    |> M.mapOrElse
                        (\p ->
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
                        )
                        []
               )
        )
    )
