module Models.Project.Storage exposing (Storage(..), decode, encode, icon, title, viewLabel)

import FontAwesome.Icon exposing (Icon, viewIcon)
import FontAwesome.Solid as Icon
import Html exposing (Html, label)
import Html.Attributes as HA
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type Storage
    = Repository
    | Server
    | LocalStorage


encode : Storage -> Value
encode value =
    case value of
        Repository ->
            JE.string "repository"

        Server ->
            JE.string "server"

        LocalStorage ->
            JE.string "local-storage"


decodeHelp : String -> Decoder Storage
decodeHelp value =
    case value of
        "repository" ->
            JD.succeed Repository

        "server" ->
            JD.succeed Server

        "local-storage" ->
            JD.succeed LocalStorage

        _ ->
            JD.fail ("Invalid value " ++ value)


decode : Decoder Storage
decode =
    JD.andThen decodeHelp JD.string


icon : Storage -> Icon
icon value =
    case value of
        LocalStorage ->
            Icon.locationArrow

        Server ->
            Icon.server

        Repository ->
            Icon.receipt


title : Storage -> String
title value =
    case value of
        LocalStorage ->
            "stored in local storage of browser"

        Server ->
            "stored in server"

        Repository ->
            "stored in repository"


viewLabel : Storage -> Html msg
viewLabel value =
    label [ HA.title (title value), HA.style "width" "24px" ] [ viewIcon (icon value) ]
