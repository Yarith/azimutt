module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , replaceUrlSelection
    , subscriptions
    , update
    )

import Browser.Navigation
import Gen.Route as Route exposing (Route)
import Json.Decode as Json
import PagesComponents.App.Models exposing (Preselect)
import Request exposing (Request)
import Url.Builder exposing (QueryParameter)


type alias Flags =
    Json.Value


type alias Model =
    { isDev : Bool }


type Msg
    = PushRoute Route (List QueryParameter)


replaceUrlSelection : Maybe Preselect -> Msg
replaceUrlSelection preselect =
    PushRoute Route.App <|
        case preselect of
            Nothing ->
                []

            Just { projectId, layoutName } ->
                List.concat
                    [ [ Url.Builder.string "projectId" projectId ]
                    , case layoutName of
                        Just value ->
                            [ Url.Builder.string "layoutName" value ]

                        Nothing ->
                            []
                    ]


decodeIsDev : Flags -> Bool
decodeIsDev flags =
    Json.decodeValue (Json.field "isDev" Json.bool) flags
        |> Result.withDefault False


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { isDev = decodeIsDev flags }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update { key } msg model =
    case msg of
        PushRoute route query ->
            ( model
            , Browser.Navigation.replaceUrl key (Route.toHref route ++ Url.Builder.toQuery query)
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
