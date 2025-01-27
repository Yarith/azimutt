module Libs.Models.Position exposing (Position, add, decode, diff, div, encode, fromTuple, mult, negate, sub, toTuple)

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Libs.Json.Encode as E


type alias Position =
    { left : Float, top : Float }


fromTuple : ( Float, Float ) -> Position
fromTuple ( left, top ) =
    Position left top


toTuple : Position -> ( Float, Float )
toTuple pos =
    ( pos.left, pos.top )


add : Position -> Position -> Position
add delta pos =
    Position (pos.left + delta.left) (pos.top + delta.top)


sub : Position -> Position -> Position
sub delta pos =
    Position (pos.left - delta.left) (pos.top - delta.top)


mult : Float -> Position -> Position
mult factor pos =
    Position (pos.left * factor) (pos.top * factor)


div : Float -> Position -> Position
div factor pos =
    Position (pos.left / factor) (pos.top / factor)


negate : Position -> Position
negate pos =
    Position -pos.left -pos.top


diff : Position -> Position -> ( Float, Float )
diff to from =
    ( from.left - to.left, from.top - to.top )


encode : Position -> Value
encode value =
    E.object
        [ ( "left", value.left |> Encode.float )
        , ( "top", value.top |> Encode.float )
        ]


decode : Decode.Decoder Position
decode =
    Decode.map2 Position
        (Decode.field "left" Decode.float)
        (Decode.field "top" Decode.float)
