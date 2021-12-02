module PagesComponents.App.Updates.Source exposing (handleSource)

import Conf exposing (schemaSamples)
import Dict
import Effect exposing (Effect)
import Libs.Bool as B
import Libs.Maybe as M
import Models.Project as Project
import PagesComponents.App.Models exposing (Model, Msg(..), SourceMsg(..))
import PagesComponents.App.Updates.Helpers exposing (setProject, setSwitch)
import PagesComponents.App.Updates.Project exposing (createProject, updateProject)
import Ports exposing (observeTablesSize, readLocalFile, readRemoteFile, toastError, toastInfo)


handleSource : SourceMsg -> Model -> ( Model, Effect Msg )
handleSource msg model =
    case msg of
        FileDragOver _ _ ->
            ( model, Effect.none )

        FileDragLeave ->
            ( model, Effect.none )

        LoadLocalFile project source file ->
            ( model |> setSwitch (\s -> { s | loading = True }), Effect.fromCmd <| readLocalFile project source file )

        LoadRemoteFile project source url ->
            ( model, Effect.fromCmd <| readRemoteFile project source url Nothing )

        LoadSample name ->
            ( model
            , schemaSamples
                |> Dict.get name
                |> M.mapOrElse (\( _, url ) -> readRemoteFile Nothing Nothing url (Just name))
                    (toastError ("Sample <b>" ++ name ++ "</b> not found"))
                |> Effect.fromCmd
            )

        FileLoaded projectId sourceInfo content ->
            model.project
                |> M.filter (\project -> project.id == projectId)
                |> M.mapOrElse
                    (\project -> project |> updateProject sourceInfo content |> Tuple.mapFirst (\p -> { model | project = Just p }))
                    (model |> createProject projectId sourceInfo content)

        ToggleSource source ->
            ( model |> setProject (Project.updateSource source.id (\s -> { s | enabled = not s.enabled }))
            , Effect.fromCmd <|
                Cmd.batch
                    [ observeTablesSize (model.project |> M.mapOrElse (\p -> p.layout.tables |> List.map .id) [])
                    , toastInfo ("Source <b>" ++ source.name ++ "</b> set to " ++ B.cond source.enabled "hidden" "visible" ++ ".")
                    ]
            )

        CreateSource source message ->
            ( model |> setProject (Project.addSource source), Effect.fromCmd <| toastInfo message )

        DeleteSource source ->
            ( model |> setProject (Project.deleteSource source.id), Effect.fromCmd <| toastInfo ("Source <b>" ++ source.name ++ "</b> has been deleted from project.") )
