module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { isDev : Bool }


type Msg
    = NoOp


decodeIsDev : Flags -> Bool
decodeIsDev flags =
    Json.decodeValue (Json.field "isDev" Json.bool) flags
        |> Result.withDefault False


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { isDev = decodeIsDev flags }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
