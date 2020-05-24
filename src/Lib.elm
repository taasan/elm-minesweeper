module Lib exposing
    ( between
    , bool2int
    , isEven
    , isOdd
    , onContextMenu
    , random
    )

import Bitwise exposing (and)
import Json.Decode as Json
import Random exposing (Generator)
import Svg exposing (Attribute)
import Svg.Events exposing (preventDefaultOn)


isEven : Int -> Bool
isEven n =
    and n 1 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


onContextMenu : msg -> Attribute msg
onContextMenu msg =
    preventDefaultOn "contextmenu" (Json.succeed ( msg, True ))


random : Generator Int
random =
    Random.int Random.minInt Random.maxInt


between : ( Int, Int ) -> Int -> Bool
between ( a, b ) x =
    let
        a_ =
            min a b

        b_ =
            max a b
    in
    x >= a_ && x <= b_


bool2int : Bool -> Int
bool2int b =
    if b then
        1

    else
        0
