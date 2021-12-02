module PagesComponents.App.Updates.Layout exposing (handleLayout)

import Dict
import Effect exposing (Effect)
import Libs.Bool as B
import Libs.Dict as D
import Libs.Maybe as M
import Models.Project exposing (Project)
import Models.Project.Layout as Layout
import Models.Project.LayoutName exposing (LayoutName)
import PagesComponents.App.Models exposing (LayoutMsg(..), Model, Msg)
import PagesComponents.App.Updates.Helpers exposing (setLayout, setLayouts, setProjectWithCmd)
import PagesComponents.App.Updates.Project as Project
import Ports exposing (activateTooltipsAndPopovers, observeTablesSize, saveProject, track)
import Time
import Tracking exposing (events)


type alias Model x =
    { x
        | newLayout : Maybe LayoutName
        , project : Maybe Project
    }


handleLayout : LayoutMsg -> Model x -> ( Model x, Effect Msg )
handleLayout msg model =
    case msg of
        LNew name ->
            ( { model | newLayout = B.cond (String.length name == 0) Nothing (Just name) }, Effect.none )

        LCreate name ->
            { model | newLayout = Nothing } |> setProjectWithCmd (createLayout name)

        LLoad name ->
            model |> setProjectWithCmd (loadLayout name)

        LUnload ->
            model |> setProjectWithCmd unloadLayout

        LUpdate name ->
            model |> setProjectWithCmd (updateLayout name)

        LDelete name ->
            model |> setProjectWithCmd (deleteLayout name)


createLayout : LayoutName -> Project -> ( Project, Effect Msg )
createLayout name project =
    -- TODO check that layout name does not already exist
    { project | usedLayout = Just name }
        |> setLayouts (Dict.update name (\_ -> Just project.layout))
        |> (\newSchema ->
                ( newSchema
                , Effect.batch
                    [ Effect.fromCmd <|
                        Cmd.batch
                            [ saveProject newSchema
                            , track (events.createLayout project.layout)
                            ]
                    , Project.updateProjectUrl newSchema
                    ]
                )
           )


loadLayout : LayoutName -> Project -> ( Project, Effect Msg )
loadLayout name project =
    project.layouts
        |> Dict.get name
        |> M.mapOrElse
            (\layout ->
                let
                    updatedProject =
                        { project | usedLayout = Just name } |> setLayout (\_ -> layout)
                in
                ( updatedProject
                , Effect.batch
                    [ Effect.fromCmd <|
                        Cmd.batch
                            [ layout.tables |> List.map .id |> observeTablesSize
                            , activateTooltipsAndPopovers
                            , track (events.loadLayout layout)
                            ]
                    , Project.updateProjectUrl updatedProject
                    ]
                )
            )
            ( project, Effect.none )


unloadLayout : Project -> ( Project, Effect Msg )
unloadLayout project =
    let
        updatedProject =
            { project | usedLayout = Nothing }
    in
    ( updatedProject, Project.updateProjectUrl updatedProject )


updateLayout : LayoutName -> Project -> ( Project, Effect Msg )
updateLayout name project =
    -- TODO check that layout name already exist
    { project | usedLayout = Just name }
        |> setLayouts (Dict.update name (\_ -> Just project.layout))
        |> (\newSchema -> ( newSchema, Effect.fromCmd <| Cmd.batch [ saveProject newSchema, track (events.updateLayout project.layout) ] ))


deleteLayout : LayoutName -> Project -> ( Project, Effect Msg )
deleteLayout name project =
    { project | usedLayout = B.cond (project.usedLayout == Just name) Nothing (Just name) }
        |> setLayouts (Dict.update name (\_ -> Nothing))
        |> (\newSchema ->
                ( newSchema
                , Effect.batch
                    [ Effect.fromCmd <|
                        Cmd.batch
                            [ saveProject newSchema
                            , track (events.deleteLayout (project.layouts |> D.getOrElse name (Layout.init (Time.millisToPosix 0))))
                            ]
                    , Project.updateProjectUrl newSchema
                    ]
                )
           )
