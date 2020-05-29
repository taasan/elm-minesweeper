module Lib exposing
    ( between
    , bool2int
    , flip
    , isEven
    , isOdd
    )

import Bitwise exposing (and)
import Random exposing (Generator)


isEven : Int -> Bool
isEven n =
    and n 1 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


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


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x
